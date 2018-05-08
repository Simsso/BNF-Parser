import qualified BackusNaurForm as BNF

import Text.Trifecta

main = do
  s <- readFile "BNFDefinition.txt"
  putStrLn $ show $ parseString BNF.syntax mempty s