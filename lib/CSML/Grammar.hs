module CSML.Grammar where

{-

An example grammar for the Simply Typed Lambda Calculus


-- this is a grammatical type
Type A, B, C
  ::= "Unit"     (unit_type)
    | A "*" B    (product_type)
    | A "+" B    (sum_type)
    | A "->" B   (function_type)
    ;

-- this is another grammatical type
-- but it has lots of constructors
Term M, N, P, D
  ::= "<" ">"                 (unit)
    | "<" M "," N ">"         (pair)
    | "fst" P                 (fst)
    | "snd" P                 (snd)
    | "left" M                (left)
    | "right" N               (right)
    | "case" D "of" "{"
        "left" x "->" M ";"
        "right" y "->" N "}"   (case)
    | "\\" x "->" M           (lambda)
    | M N                     (application)
    | M ":" A                 (annotation)
    | x                       (variable)
    ;

-- this one has only one constructor
-- which is useful for making lexical forms
-- into a category that exists in the tree
-- rather than being omitted like a CST node
VariableName x, y, z ::= vn (variable_name);

-- this is a synonym
vn = /[a-z][a-zA-Z0-9_]*/;



In this grammar we can see

- syntax types
    - which have names, variables, and rules
    - rules have right hand sides consisting of lexemes and variables, and a rule name
- lexeme synonyms
- end of line comments


-}

type Grammar = [Clause]

data Clause
    = SyntaxTypeClause TypeName [VariableName] [Rule]
    | LexemeSynonymClause VariableName Lexeme
    deriving (Show, Eq)

syntaxTypeClause :: String -> [String] -> [Rule] -> Clause
syntaxTypeClause tn vns =
    SyntaxTypeClause
        (TypeName tn)
        (map VariableName vns)

lexemeSynonymClause :: String -> Lexeme -> Clause
lexemeSynonymClause vn =
    LexemeSynonymClause
        (VariableName vn)

newtype TypeName = TypeName String
    deriving (Show, Eq)

newtype VariableName = VariableName String
    deriving (Show, Eq)

data Rule
    = Rule RuleName [RulePart]
    deriving (Show, Eq)

rule :: String -> [RulePart] -> Rule
rule rn = Rule (RuleName rn)

newtype RuleName = RuleName String
    deriving (Show, Eq)

data RulePart
    = VariablePart VariableName
    | LexemePart Lexeme
    deriving (Show, Eq)

v :: String -> RulePart
v x = VariablePart (VariableName x)

l :: Lexeme -> RulePart
l = LexemePart

data Lexeme
    = StringLexeme String
    | RegexLexeme Regex
    deriving (Show, Eq)

slex :: String -> Lexeme
slex = StringLexeme

rlex :: String -> Lexeme
rlex r = RegexLexeme (Regex r)

newtype Regex = Regex String
    deriving (Show, Eq)



-- grammarToTypeNa

type VariableInfoMapping = [(String, NameInfo)]

data NameInfo
    = SyntaxTypeVariable TypeName
    | LexemeVariable Lexeme

grammarToVariableInfoMapping :: Grammar -> VariableInfoMapping
grammarToVariableInfoMapping g =
    g >>= clauseToVariableInfoMapping

clauseToVariableInfoMapping :: Clause -> VariableInfoMapping
clauseToVariableInfoMapping (SyntaxTypeClause tn vns _) =
    [ (vn,SyntaxTypeVariable tn) | VariableName vn <- vns ]
clauseToVariableInfoMapping (LexemeSynonymClause (VariableName vn) lx) =
    [ (vn,LexemeVariable lx) ]