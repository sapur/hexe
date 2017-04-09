module HistorySpec where

import Test.Hspec
import Test.QuickCheck

import qualified History as T


spec = do

    describe "empty" $ do

        it "empty history looks empty" $
            T.getStats T.emptyHistory `shouldBe` (0,0)

        it "trimmed empty history is empty" $
            T.trim T.emptyHistory `shouldBe` (T.emptyHistory :: T.History Int)

    it "undo/redo duality" $ property $
        \cur0 hist0 ->
            let _             = hist0 :: T.History Int
                hist1         = T.setVersion cur0 hist0
                (cur2, hist2) = T.undo cur0 hist1
                (cur3, hist3) = T.redo cur2 hist2
            in  cur3 === cur0


makeHist :: Arbitrary a => [Int] -> [a] -> T.History a
makeHist opNs curs =
    let ops1 = map histOp opNs
        ops2 = zipWith ($) ops1 curs
        hist = foldr ($) T.emptyHistory ops2
    in  hist

histOp n cur hist = case n `mod` 3 of
    0 -> T.setVersion cur hist
    1 -> snd $ T.undo cur hist
    2 -> snd $ T.redo cur hist


instance Arbitrary a => Arbitrary (T.History a) where
    arbitrary = makeHist <$> arbitrary <*> arbitrary
