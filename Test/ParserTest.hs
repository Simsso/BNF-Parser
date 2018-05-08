module Test.ParserTest where
  
import BackusNaurForm
import Test.Hspec
import Text.Trifecta (parseString, Result(..))
import Util (runParser)

shouldMatch :: Eq a => Result a -> a -> Bool
shouldMatch (Success x) a = x == a
shouldMatch (Failure _) _ = False

main :: IO ()
main = hspec $ do
  
  describe "lineEnd" $ do
    runLineEnd <- pure $ runParser lineEnd
    it "should accept a newline \"\\n\"" $ do
      runLineEnd "\n" `shouldBe` True
    it "should accept muliple newlines" $ do
      runLineEnd "\n\n" `shouldBe` True
    it "should ignore leading whitespaces" $ do
      runLineEnd "  \n \n" `shouldBe` True
  
  describe "ruleChar" $ do
    runRuleChar <- pure $ runParser ruleChar
    runRuleCharParser <- pure $ parseString ruleChar mempty
    it "should accept a letter" $ do
      runRuleChar "a" `shouldBe` True
    it "should accept a digit" $ do
      runRuleChar "5" `shouldBe` True
    it "should accept a dash" $ do
      runRuleChar "-" `shouldBe` True
    it "should reject special chars" $ do
      runRuleChar "%" `shouldBe` False
    it "should reject empty strings" $ do
      runRuleChar "" `shouldBe` False
    it "should parse the character" $ do
      runRuleCharParser "x" `shouldMatch` 'x'

  describe "ruleName" $ do
    runRuleName <- pure $ runParser ruleName
    runRuleNameParser <- pure $ parseString ruleName mempty
    it "should reject empty strings" $ do
      runRuleName "" `shouldBe` False
    it "should reject strings starting with a number" $ do
      runRuleName "1bdf" `shouldBe` False
    it "should accept single char strings" $ do
      runRuleName "a" `shouldBe` True
    it "should accept multi char strings" $ do
      runRuleName "abc" `shouldBe` True
    it "should accept strings starting with a char" $ do
      runRuleName "abc23" `shouldBe` True
    it "should parse the string" $ do
      runRuleNameParser "a123" `shouldMatch` "a123"
  
  describe "character1" $ do
    runCharacter1 <- pure $ runParser character1
    runCharacter1Parser <- pure $ parseString character1 mempty
    it "should accept non special chars" $ do
      runCharacter1 "x" `shouldBe` True
    it "should accept special chars" $ do
      runCharacter1 "!" `shouldBe` True
    it "should accept single quotes" $ do
      runCharacter1 "'" `shouldBe` True
    it "should reject double quotes" $ do
      runCharacter1 "\"" `shouldBe` False
    it "should parse the character" $ do
      runCharacter1Parser "a" `shouldMatch` 'a'
  
  describe "character2" $ do
    runCharacter2 <- pure $ runParser character2
    runCharacter2Parser <- pure $ parseString character2 mempty
    it "should accept non special chars" $ do
      runCharacter2 "x" `shouldBe` True
    it "should accept special chars" $ do
      runCharacter2 "!" `shouldBe` True
    it "should accept double quotes" $ do
      runCharacter2 "\"" `shouldBe` True
    it "should reject single quotes" $ do
      runCharacter2 "'" `shouldBe` False
    it "should parse the character" $ do
      runCharacter2Parser "a" `shouldMatch` 'a'

  describe "literal" $ do
    runLiteral <- pure $ runParser literal
    runLiteralParser <- pure $ parseString literal mempty
    it "should parse chars in double quotes" $ do
      runLiteralParser "\"abc\"" `shouldMatch` Literal "abc"
    it "should parse chars in single quotes" $ do
      runLiteralParser "'abc'" `shouldMatch` Literal "abc"
    it "should parse chars in double quotes containing single quotes" $ do
      runLiteralParser "\"a'bc\"" `shouldMatch` Literal "a'bc"
    it "should parse chars in single quotes containing double quotes" $ do
      runLiteralParser "'a\"bc'" `shouldMatch` Literal "a\"bc"
    it "should reject empty strings" $ do
      runLiteral "" `shouldBe` False
      
  describe "term" $ do
    runTerm <- pure $ parseString term mempty
    it "should parse valid literals" $ do
      runTerm "'abcdef'" `shouldMatch` Literal "abcdef"
    it "should parse valid rules" $ do
      runTerm "<abc>" `shouldMatch` (Rule $ RuleName "abc")

  describe "list" $ do
    runList <- pure $ parseString list mempty
    it "should parse a single term" $ do
      runList "'abc'" `shouldMatch` [Literal "abc"]
    it "should parse multiple rules" $ do
      runList "<abc><def>" `shouldMatch` 
        [Rule $ RuleName "abc", Rule $ RuleName "def"]
    it "should parse multiple terms" $ do
      runList "'abc'\"x\"" `shouldMatch` 
        [Literal "abc", Literal "x"]
    it "should parse mixed rules and literals" $ do
      runList "<abc><def>'x'<abc>'literal'" `shouldMatch` 
        [Rule $ RuleName "abc", Rule $ RuleName "def", 
         Literal "x", Rule $ RuleName "abc", Literal "literal"]
    it "should be insensitive to whitespaces" $ do
      runList "'abc' \"x\"" `shouldMatch` 
        [Literal "abc", Literal "x"]
         
  describe "expression" $ do
    runDescription <- pure $ parseString expression mempty
    it "should parse a single term" $ do
      runDescription "'abc'" `shouldMatch` Expression [[Literal"abc"]]
    it "should parse terms connected by \"or\", i.e. \"|\"" $ do
      runDescription "'abc'|'def'" `shouldMatch` Expression 
        [[Literal "abc"], [Literal "def"]]
    it "should parse complex terms connected by \"or\"" $ do
      runDescription "'abc'<x>|<y>'def'" `shouldMatch` Expression 
        [[Literal "abc", Rule $ RuleName "x"],
         [Rule $ RuleName "y", Literal "def"]]
    it "should be insensitive to whitespaces around \"|\"" $ do
      runDescription "'abc'  <x> | <y>  'def'" `shouldMatch` Expression 
        [[Literal "abc", Rule $ RuleName "x"],
         [Rule $ RuleName "y", Literal "def"]]
    it "should be insensitive to whitespaces around \"|\" with multiple \"or\"s" $ do
      runDescription "'x' | 'y' | 'z'" `shouldMatch` Expression 
        [[Literal "x"], [Literal "y"], [Literal "z"]]
  
  describe "rule" $ do
    runRule <- pure $ parseString rule mempty
    it "should parse a simple rule" $ do
      runRule "<ab>::=<ab>" `shouldMatch` RuleDefinition 
        (RuleName "ab") (Expression [[Rule $ RuleName "ab"]])
    it "should parse a simple rule with rule name" $ do
      runRule "<ab>::='x'" `shouldMatch` RuleDefinition 
        (RuleName "ab") (Expression [[Literal "x"]])
    it "should parse a rule ending with a new line" $ do
      runRule "<ab>::='x'\n" `shouldMatch` RuleDefinition 
        (RuleName "ab") (Expression [[Literal "x"]])
    it "should be insensitive to whitespaces" $ do
      runRule " <ab>   ::=   'x'  " `shouldMatch` RuleDefinition 
        (RuleName "ab") (Expression [[Literal "x"]])
        
  describe "syntax" $ do 
    runSyntax <- pure $ parseString syntax mempty
    it "should parse two lines of rules" $ do
      runSyntax "<a>::='b'\n<c>::=<d>" `shouldMatch` BNFDefinition
        [RuleDefinition (RuleName "a") (Expression [[Literal "b"]]),
         RuleDefinition (RuleName "c") (Expression [[Rule $ RuleName "d"]])]
