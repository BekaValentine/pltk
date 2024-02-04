module CSML.CFG where

import Data.List (intercalate, nub)

import qualified CSML.Grammar as CSML
import CSML.TextTools


newtype CFG
    = CFG [Rule]
    deriving (Show, Eq)

newtype NonTerminal = NonTerminal String
    deriving (Show, Eq)

data Terminal
    = StringTerminal String
    | RegexTerminal String
    deriving (Show, Eq)

data Rule = Rule NonTerminal [RulePart]
    deriving (Show, Eq)

data RulePart
    = TerminalPart Terminal
    | NonTerminalPart NonTerminal
    deriving (Show, Eq)


cfgToBNF :: CFG -> String
cfgToBNF g@(CFG rules) = 
    unlines' $ do
        nt <- nts
        let rhsesForNT = [ rhs | Rule lhs rhs <- rules, lhs == nt ]
        [ rulesForNTToBNF nt rhsesForNT ]
    where
        nts = nonTerminals g

nonTerminals :: CFG -> [NonTerminal]
nonTerminals (CFG rules) = nub [ nt | Rule nt _ <- rules ]

rulesForNTToBNF :: NonTerminal -> [[RulePart]] -> String
rulesForNTToBNF (NonTerminal nt) rhses =
    unlines' $
        ("<" ++ nt ++ ">") :
        ("  ::= " ++ head rhsBNFs) :
        map ("    | " ++) (tail rhsBNFs)
    where
        rhsBNFs = map rhsToBNF rhses

rhsToBNF :: [RulePart] -> String
rhsToBNF rhs = unwords (map rulePartToBNF rhs)

rulePartToBNF :: RulePart -> String
rulePartToBNF (TerminalPart t) = terminalToBNF t
rulePartToBNF (NonTerminalPart nt) = nonTerminalToBNF nt

terminalToBNF :: Terminal -> String
terminalToBNF (StringTerminal s) = show s
terminalToBNF (RegexTerminal r) = "/" ++ r ++ "/"

nonTerminalToBNF :: NonTerminal -> String
nonTerminalToBNF (NonTerminal nt) = "<" ++ nt ++ ">"


grammarToCFG :: CSML.Grammar -> CFG
grammarToCFG g = CFG (g >>= clauseToRules vim)
    where
        vim = CSML.grammarToVariableInfoMapping g

clauseToRules :: CSML.VariableInfoMapping -> CSML.Clause -> [Rule]
clauseToRules vim (CSML.SyntaxTypeClause _ (CSML.TypeName tn) _ rules) =
    let (rns, rs) = unzip (map (ruleToRuleNameAndRule vim) rules)
        typeRules =
            [ Rule (NonTerminal tn) [NonTerminalPart (NonTerminal rn)]
            | rn <- rns
            ] 
    in typeRules ++ rs
clauseToRules _ (CSML.LexemeSynonymClause _ _ _) = []

ruleToRuleNameAndRule :: CSML.VariableInfoMapping -> CSML.Rule -> (String,Rule)
ruleToRuleNameAndRule vim (CSML.Rule _ (CSML.RuleName rn) parts) =
    (rn, Rule (NonTerminal rn) [ partToPart vim part | part <- parts ])

partToPart :: CSML.VariableInfoMapping -> CSML.RulePart -> RulePart
partToPart vim (CSML.VariablePart (CSML.VariableName vn)) =
    case lookup vn vim of
        Nothing -> error "This shouldn't happen! Naughty!"
        Just (CSML.SyntaxTypeVariable (CSML.TypeName tn)) ->
            NonTerminalPart (NonTerminal tn)
        Just (CSML.LexemeVariable lx) -> TerminalPart (lexemeToPart lx)
partToPart _ (CSML.LexemePart lx) = TerminalPart (lexemeToPart lx)

lexemeToPart :: CSML.Lexeme -> Terminal
lexemeToPart (CSML.StringLexeme s) = StringTerminal s
lexemeToPart (CSML.RegexLexeme (CSML.Regex r)) = RegexTerminal r