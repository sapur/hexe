module Commands.LineEditor (
    beginInput, cancelInput, commitInput,
    addInput, moveInput, deleteInput,
) where

import Editor

import qualified Buffer as Buf

import Commands.General


beginInput check accept imm mbKey thisKm nextKm action = do
    withEditor $ \ed -> ed
        { edPending = Just $ mkInputEditor check accept imm nextKm action
        }
    case mbKey of
        Nothing -> setKeymap thisKm
        Just key -> forwardKeymap thisKm key

cancelInput = withEditor $ \ed
   -> setCursor (edCursor ed)
    $ ed { edPending = Nothing
         , edBuffer  = Buf.removeSlack (edCursor ed) (edBuffer ed)
         }

commitInput = do
    mbPending <- withEditorSt $ \ed ->
        (edPending ed, ed{ edPending = Nothing })
    case mbPending of
        Nothing  -> return ()  -- FIXME
        Just inp -> do
            setKeymap (inpNextKm inp)
            (inpAction inp) (inpText inp)
            withEditor $ \ed
                -> setCursor (edCursor ed)
                 $ ed{ edBuffer = Buf.removeSlack (edCursor ed) (edBuffer ed)
                     }

withInput f = withEditor $ \ed -> ed{ edPending = mod (edPending ed) }  where
    mod (Just inp) = Just (f inp)
    mod Nothing    = Nothing

addInput ch = withInput $ \inp ->
    let txt = inpText inp
        cur = inpCursor inp
    in  inp { inpText   = (take cur txt ++ [ch] ++ drop cur txt)
            , inpCursor = cur + 1
            }

moveInput d = withInput $ clampInput . \inp ->
    inp{ inpCursor = inpCursor inp + d }

deleteInput d = withInput $ \inp ->
    let txt = inpText inp
        cur = inpCursor inp
    in  clampInput $ case d of
            _ | d < 0 -> inp{ inpCursor = cur - 1
                            , inpText   = take (cur-1) txt ++ drop cur txt
                            }
            _ | d > 0 -> inp{ inpText   = take cur txt ++ drop (cur+1) txt
                            }
            _         -> inp

clampInput inp =
    let txt = inpText inp
        cur = min (length txt) (max 0 (inpCursor inp))
    in  inp { inpCursor = cur
            }
