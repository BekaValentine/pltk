module CSML.CSMLParser (parseSource) where

import Control.Monad (void)
import Control.Monad.Combinators
    ( (<|>), many, manyTill, sepBy1, some )
import Data.Void ( Void )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP hiding (space)
import qualified Text.Megaparsec.Char.Lexer as MP

import qualified CSML.Grammar as CSML



type Parser = MP.Parsec CSMLParseError SourceCode
type CSMLParseError = Void
type SourceCode = String



parseSource :: String
            -> String
            -> Either (MP.ParseErrorBundle String CSMLParseError) CSML.Grammar
parseSource =
    MP.parse (spaceWithComments *> grammar <* MP.eof)



spaceWithComments :: Parser ()
spaceWithComments =
    MP.space
        MP.space1
        (MP.skipLineComment "--")
        (MP.skipBlockComment "{-" "-}")

symbol :: Parser a -> Parser a
symbol = MP.lexeme spaceWithComments

symbol_ :: String -> Parser ()
symbol_ = void . MP.symbol spaceWithComments

comma :: Parser ()
comma = symbol_ ","

pipe :: Parser ()
pipe = symbol_ "|"

coloncolonequals :: Parser ()
coloncolonequals = symbol_ "::="

semicolon :: Parser ()
semicolon = symbol_ ";"

lpar :: Parser ()
lpar = symbol_ "("

rpar :: Parser ()
rpar = symbol_ ")"

parens :: Parser a -> Parser a
parens = MP.between lpar rpar

equals :: Parser ()
equals = symbol_ "="



grammar :: Parser CSML.Grammar
grammar = some clause

clause :: Parser CSML.Clause
clause = syntaxTypeClause <|> lexemeSynonymClause

syntaxTypeClause :: Parser CSML.Clause
syntaxTypeClause = do
    tn <- typeName
    vns <- sepBy1 variableName comma
    _ <- coloncolonequals
    rs <- sepBy1 rule pipe
    _ <- semicolon
    return (CSML.SyntaxTypeClause tn vns rs)

lexemeSynonymClause :: Parser CSML.Clause
lexemeSynonymClause = do
    vn <- variableName
    _ <- equals
    lx <- lexeme
    _ <- semicolon
    return (CSML.LexemeSynonymClause vn lx)

typeName :: Parser CSML.TypeName
typeName = CSML.TypeName <$> rawTypeName
    where
        rawTypeName :: Parser String
        rawTypeName = symbol $ do
            c <- MP.upperChar
            cs <- many (MP.alphaNumChar <|> MP.single '_')
            return (c:cs)

variableName :: Parser CSML.VariableName
variableName = CSML.VariableName <$> rawVariableName
    where
        rawVariableName :: Parser String
        rawVariableName = symbol $ do
            c <- MP.letterChar
            cs <- many (MP.alphaNumChar <|> MP.single '_')
            return (c:cs)

rule :: Parser CSML.Rule
rule = do parts <- some rulePart
          rn <- parens ruleName
          return (CSML.Rule rn parts)

ruleName :: Parser CSML.RuleName
ruleName = CSML.RuleName <$> rawRuleName
    where
        rawRuleName :: Parser String
        rawRuleName = symbol $ do
            c <- MP.letterChar
            cs <- many (MP.alphaNumChar <|> MP.single '_')
            return (c:cs)

rulePart :: Parser CSML.RulePart
rulePart = variablePart <|> lexemePart

variablePart :: Parser CSML.RulePart
variablePart = CSML.VariablePart <$> variableName

lexemePart :: Parser CSML.RulePart
lexemePart = CSML.LexemePart <$> lexeme

lexeme :: Parser CSML.Lexeme
lexeme = stringLexeme <|> regexLexeme

stringLexeme :: Parser CSML.Lexeme
stringLexeme = CSML.StringLexeme <$> string

regexLexeme :: Parser CSML.Lexeme
regexLexeme = CSML.RegexLexeme <$> regex

string :: Parser String
string = symbol (MP.char '"' >> manyTill MP.charLiteral (MP.char '"'))

regex :: Parser CSML.Regex
regex = CSML.Regex <$> rawRegex
    where
        rawRegex :: Parser String
        rawRegex = symbol (MP.char '/' >> manyTill MP.charLiteral (MP.char '/'))