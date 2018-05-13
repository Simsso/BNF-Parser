{-# OPTIONS_GHC -Wall #-}

module Parser.BNF.Util where

import Parser.BNF.Constants
import Test.QuickCheck (Arbitrary, arbitrary, choose, frequency, Gen, sized)
import Text.Megaparsec.String
  
c2s :: Char -> String
c2s = (:[])

cp2s :: Parser Char -> Parser String
cp2s = fmap (:[])


-- generation utility methods
freqFromChars :: [Char] -> Gen Char
freqFromChars c = frequency $ map((,) 1 . return) c

-- list generation from https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
arbitraryList :: (Arbitrary a) => (Int, Int) -> Gen [a]
arbitraryList = flip genList arbitrary

genList :: (Int, Int) -> Gen a -> Gen [a]
genList (minL, maxL) g = sized $ \n -> do
  k <- choose (minL, min (max minL n) maxL)
  sequence [ g | _ <- [1..k] ]

ruleNameGen :: Gen String
ruleNameGen = do
    begin <- freqFromChars availableLetters
    remainder <- genList (0, 10) $ freqFromChars $ '-' : availableLetters ++ availableDigits
    return $ begin : remainder

literalGen :: Gen String
literalGen =
  let avChars = availableLetters ++ availableDigits ++ availableSpecialChars
                -- literal may contain either ' or " but never both
      listGen = genList (0, 20) . freqFromChars . flip (:) avChars
  in  frequency [(1, listGen '\''), (1, listGen '"')]