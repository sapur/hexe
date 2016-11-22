{-# LANGUAGE MultiParamTypeClasses #-}
module Buffer (
    Buffer (..), mkBuffer, length,
    Offset,
    ModState (..),
    viewRange,
    setByte, insertByte, deleteByte, insertSlack, removeSlack, extendCond,
    setMark, getMark, findMark,
    isModified,
    readFile, writeFile
) where

import Prelude hiding (length, concatMap, all, and, or, readFile, writeFile)

import Data.Foldable hiding (length)
import Data.Monoid hiding (Alt)
import Data.Word
import Debug.Trace
import Text.Printf

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Data.FingerTree (FingerTree, Measured, measure)
import qualified Data.FingerTree as FT


data Buffer = Buffer
    { bufPath     :: FilePath
    , bufChunks   :: Chunks
    , bufModified :: Bool
    }

type Chunks = FingerTree Index Chunk

data Chunk = Chunk
    { chMod  :: ModState
    , chData :: ByteString
    , chMark :: Bool
    }

data ModState
    = Copy
    | Alt
    | Ins
    | Slack
    deriving (Eq, Ord, Show)

type Offset = Int

data Index = Index
    { idxLength :: Int
    , idxMark   :: Bool
    }


instance Monoid Index where
    mempty                            = Index 0 False
    Index lA mA `mappend` Index lB mB = Index (lA+lB) (mA || mB)

instance Measured Index Chunk where
    measure ch = Index (BS.length $ chData ch) (chMark ch)


mkBuffer path = Buffer
    { bufPath     = path
    , bufChunks   = FT.empty
    , bufModified = False
    }

length = idxLength . FT.measure . bufChunks


mkChunk mod bin = Chunk
    { chMod  = mod
    , chData = bin
    , chMark = False
    }

mergeable chunkA chunkB = and
    [ chMod chunkA == chMod chunkB
    , not (chMark chunkA || chMark chunkB)
    ]


viewRange :: Offset -> Int -> Buffer -> [(Word8, ModState, Bool)]
viewRange offset count buf = concatMap toBytes chunks  where
    (_,trimLeft) = splitChunksAt offset (bufChunks buf)
    (chunks,_)   = splitChunksAt count  trimLeft
    toBytes ch   = map (\b -> (b, chMod ch, chMark ch)) (BS.unpack $ chData ch)


setByte offset b buf = setModified buf'  where
    buf'      = buf{ bufChunks = assertChunks chunks' }
    chunks'   = modifyChunks offset 1 set (bufChunks buf)
    set []    = []
    set [old] = let mod = max Alt (min Ins (chMod old))
                in  [mkChunk mod (BS.singleton b)]
    set chs   = error $ printf "Oops, %s should never happen" (show chs)

insertByte offset b buf = setModified buf'  where
    (before, after) = splitChunksAt offset (bufChunks buf)
    after'          = mkChunk Ins (BS.singleton b) FT.<| after
    chunks'         = before >$< after'
    buf'            = buf{ bufChunks = assertChunks chunks' }

deleteByte offset buf = setModified buf'  where
    buf'    = buf{ bufChunks = assertChunks chunks' }
    chunks' = modifyChunks offset 1 del (bufChunks buf)
    del [_] = []
    del chs = error $ printf "Oops, %s should never happen" (show chs)

insertSlack offset count buf = buf{ bufChunks = assertChunks chunks' }  where
    (before, after) = splitChunksAt offset (bufChunks buf)
    new             = FT.singleton $ mkChunk Slack (BS.replicate count 0)
    chunks'         = before >$< new >$< after

removeSlack offset buf = buf{ bufChunks = assertChunks chunks' }  where
    chunks       = bufChunks buf
    (before, at) = splitChunksAt offset chunks
    chunks'      = case FT.viewl at of
        this FT.:< after -> case chMod this of
            Slack -> before >$< after
            _     -> chunks
        FT.EmptyL -> chunks

extendCond offset buf
    = case (offset >= length buf, FT.viewr (bufChunks buf)) of
        (True, _ FT.:> ch) | chMod ch /= Slack
            -> insertSlack offset 1 buf
        (_, FT.EmptyR)
            -> insertSlack offset 1 buf
        _ -> buf


setMark offset mark buf = buf{ bufChunks = assertChunks chunks' }  where
    chunks'  = modifyChunks offset 1 set (bufChunks buf)
    set [ch] = [ch{ chMark = mark }]
    set chs  = error $ printf "Oops, %s should never happen" (show chs)

getMark offset buf = result  where
    (before, at) = FT.split ((> offset) . idxLength) (bufChunks buf)
    result       = case FT.viewl at of
        (ch FT.:< _) -> chMark ch
        _            -> False

findMark False offset buf = findMarkForward offset (bufChunks buf)
findMark True  offset buf = result  where
    chunksR     = FT.reverse (bufChunks buf)
    revOffset o = length buf - o - 1
    result      = revOffset $ findMarkForward (revOffset offset) chunksR

findMarkForward :: Offset -> Chunks -> Offset
findMarkForward offset chunks = result  where
    (before, within)     = FT.split condI chunks
    condI (Index len mk) = len > (offset+1) && mk
    (wBefore, _)         = FT.split idxMark within
    result               = idxLength (measure before <> measure wBefore)


modifyChunks :: Offset -> Int -> ([Chunk] -> [Chunk]) -> Chunks -> Chunks
modifyChunks offset count f chunks = chunks'  where
    (prev  , at  ) = splitChunksAt offset chunks
    (within, next) = splitChunksAt count  at
    within'        = FT.fromList
                   $ mergeList $ f
                   $ toList within
    chunks'        = prev >$< (within' >$< next)

splitChunksAt :: Offset -> Chunks -> (Chunks, Chunks)
splitChunksAt offset chunks = ret  where
    (before, at) = FT.split ((> offset) . idxLength) chunks
    relOffset    = offset - idxLength (FT.measure before)
    ret = case FT.viewl at of
        FT.EmptyL ->
            (chunks, at)
        this FT.:< after ->
            let (thisBefore, thisAfter) = splitChunk relOffset this
            in  if   relOffset == 0
                then (before, at)
                else (before FT.|> thisBefore, thisAfter FT.<| after)

splitChunk offset chunk = chunks  where
    (before, at) = BS.splitAt offset (chData chunk)
    chunks       = (chunk{ chData = before }, chunk{ chData = at })

(>$<) = merge

merge :: Chunks -> Chunks -> Chunks
merge chunksL chunksR = case (FT.viewr chunksL, FT.viewl chunksR) of
    (ll FT.:> l, r FT.:< rr) | mergeable l r ->
        let lr = mergeChunks l r
        in  merge (ll FT.|> lr) rr
    _ -> chunksL FT.>< chunksR

mergeList :: [Chunk] -> [Chunk]
mergeList (l:r:chunks) | mergeable l r = mergeList (mergeChunks l r : chunks)
mergeList (c:chunks) = c : mergeList chunks
mergeList [] = []

mergeChunks chunkA chunkB = chunkA
    { chData = chData chunkA `BS.append` chData chunkB
    }


isModified      = bufModified

setModified buf = buf{ bufModified = True }


readFile path = BS.readFile path >>= \bin ->
    return (mkBuffer path){ bufChunks = chunksBin bin }

writeFile buf =
    let chunkList = filter isData $ toList $ bufChunks buf
        bin       = BS.concat $ map chData chunkList
        isData ch = chMod ch /= Slack
    in  BS.writeFile (bufPath buf) bin
     >> return buf{ bufChunks   = FT.fromList chunkList
                  , bufModified = False
                  }

chunksBin bin = case BS.length bin of
    0 -> FT.empty
    _ -> FT.singleton (mkChunk Copy bin)


assertChunks chunks =
    if   not (checkChunks chunks)
    then trace (show chunks) (error "assertion failed: chunks inconsistent")
    else chunks

checkChunks :: Chunks -> Bool
checkChunks chunks = noNulls && merged  where
    chunksL = toList chunks
    noNulls = all ((> 0) . BS.length . chData) chunksL
    merged  = not $ or $ zipWith mergeable chunksL (tail chunksL)


instance Show Chunk where
    show (Chunk mod bin mk) = unwords
        --[show mod, show (BS.length bin)]
        [show $ BS.unpack bin]
