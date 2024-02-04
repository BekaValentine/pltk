module Main (main) where

import qualified Text.Megaparsec as MP
import Test.Hspec
    ( hspec
    , describe
    , context
    , it
    , shouldBe
    , shouldNotSatisfy
    , shouldSatisfy
    )

import CSML.CFG
import CSML.Grammar
    ( Grammar, syntaxTypeClause, lexemeSynonymClause, rule, v, l, slex, rlex )
import qualified CSML.CSMLParser as CSML
import CSML.Elaborator

parseSucceeded :: Either a b -> Bool
parseSucceeded (Left _) = False
parseSucceeded (Right _) = True

parseSucceededWith :: Eq b => b -> Either a b -> Bool
parseSucceededWith _ (Left _) = False
parseSucceededWith x (Right y) = x == y

hasRepeatedVariables :: ElabError -> Bool
hasRepeatedVariables e =
    case e of
        RepeatedVariable _ _ -> True
        _ -> False

hasUnboundVariables :: ElabError -> Bool
hasUnboundVariables e =
    case e of
        UnboundVariable _ _ -> True
        _ -> False

parseSource :: String -> String -> Either String Grammar
parseSource fileName source =
    case CSML.parseSource fileName source of
        Left err -> Left (MP.errorBundlePretty err)
        Right x -> Right x

main :: IO ()
main = hspec $ do



    describe "The CSML parser" $ do

        context "when provided nonsense `nonsense`" $ do

            let src = "nonsense"
                actual = parseSource "test_nonsense" src

            it "parses successfully" $ shouldNotSatisfy actual parseSucceeded

        context "when provided the valid Lexical Synonym Clause `foo = \"foo\";`" $ do

            let src = "foo = \"foo\";"
                expected =
                    [ lexemeSynonymClause "foo" (slex "foo") ]
                actual = parseSource "test_lexical_synonym_clause_with_string" src

            it "parses successfully" $
                shouldSatisfy actual parseSucceeded

            it "parses correctly" $
                shouldSatisfy actual (parseSucceededWith expected)
        
        context "when provided the valid Lexical Synonym Clause `foo = /foo?/;`" $ do

            let src = "foo = /foo?/;"
                expected =
                    [ lexemeSynonymClause "foo" (rlex "foo?") ]
                actual = parseSource "test_lexical_synonym_clause_with_regex" src

            it "parses successfully" $
                shouldSatisfy actual parseSucceeded

            it "parses correctly" $
                shouldSatisfy actual (parseSucceededWith expected)
        
        context "when provided a valid Syntax Type Clause `Foo x, y ::= \"foo\" (foo);`" $ do

            let src = "Foo x, y ::= \"foo\" (foo);"
                expected =
                    [ 
                        syntaxTypeClause "Foo" ["x","y"]
                            [
                                rule "foo" [l (slex "foo")]
                            ]
                     ]
                actual = parseSource "test_syntax_type_clause" src

            it "parses successfully" $
                shouldSatisfy actual parseSucceeded

            it "parses correctly" $
                shouldSatisfy actual (parseSucceededWith expected)
        
        context "when provided the STLC" $ do

            let src = unlines
                    [ ""
                    , " -- this is a grammatical type                "
                    , " Type A, B, C                                 "
                    , "   ::= \"Unit\"     (unit_type)               "
                    , "     | A \"*\" B    (product_type)            "
                    , "     | A \"+\" B    (sum_type)                "
                    , "     | A \"->\" B   (function_type)           "
                    , "     ;                                        "
                    , "                                              "
                    , " -- this is another grammatical type          "
                    , " -- but it has lots of constructors           "
                    , " -- this is a synonym                         "
                    , " Term M, N, P, D                              "
                    , "   ::= \"<\" \">\"                 (unit)     "
                    , "     | \"<\" M \",\" N \">\"         (pair)   "
                    , "     | \"fst\" P                 (fst)        "
                    , "     | \"snd\" P                 (snd)        "
                    , "     | \"left\" M                (left)       "
                    , "     | \"right\" N               (right)      "
                    , "     | \"case\" D \"of\" \"{\"                "
                    , "         \"left\" x \"->\" M \";\"            "
                    , "         \"right\" y \"->\" N \"}\"   (case)  "
                    , "     | \"\\\\\" x \"->\" M           (lambda) "
                    , "     | M N                     (application)  "
                    , "     | M \":\" A                 (annotation) "
                    , "     | x                       (variable)     "
                    , "     ;                                        "
                    , "                                              "
                    , " {-                                           "
                    , "   this one has only one constructor          "
                    , "   which is useful for making lexical forms   "
                    , "   into a category that exists in the tree    "
                    , "   rather than being omitted like a CST node  "
                    , " -}                                           "
                    , " VariableName x, y, z ::= vn (variable_name); "
                    , "                                              "
                    , " vn = /[a-z][a-zA-Z0-9_]*/;                   "
                    ]

                expected =
                    [ syntaxTypeClause "Type" ["A", "B", "C"]
                        [ rule "unit_type" [l (slex "Unit")]
                        , rule "product_type" [v "A", l (slex "*"), v "B"]
                        , rule "sum_type" [v "A", l (slex "+"), v "B"]
                        , rule "function_type" [v "A", l (slex "->"), v "B"]
                        ]
                    , syntaxTypeClause "Term" ["M", "N", "P", "D"]
                        [ rule "unit" [l (slex "<"), l (slex ">")]
                        , rule "pair" [l (slex "<"), v "M", l (slex ","), v "N", l (slex ">")]
                        , rule "fst" [l (slex "fst"), v "P"]
                        , rule "snd" [l (slex "snd"), v "P"]
                        , rule "left" [l (slex "left"), v "M"]
                        , rule "right" [l (slex "right"), v "N"]
                        , rule "case" [ l (slex "case")
                                      , v "D"
                                      , l (slex "of")
                                      , l (slex "{")
                                      , l (slex "left")
                                      , v "x"
                                      , l (slex "->")
                                      , v "M"
                                      , l (slex ";")
                                      , l (slex "right")
                                      , v "y"
                                      , l (slex "->")
                                      , v "N"
                                      , l (slex "}")
                                      ]
                        , rule "lambda" [ l (slex "\\")
                                        , v "x"
                                        , l (slex "->")
                                        , v "M"
                                        ]
                        , rule "application" [v "M", v "N"]
                        , rule "annotation" [v "M", l (slex ":"), v "A"]
                        , rule "variable" [v "x"]
                        ]
                    , syntaxTypeClause "VariableName" ["x", "y", "z"]
                        [ rule "variable_name" [v "vn"] ]
                    , lexemeSynonymClause "vn" (rlex "[a-z][a-zA-Z0-9_]*")
                     ]
                
                actual = parseSource "test_stlc" src

            it "parses successfully" $
                shouldSatisfy actual parseSucceeded

            it "parses correctly" $
                shouldSatisfy actual (parseSucceededWith expected)
    
    describe "The elaborator" $ do

        it "errors on repeated variables" $ do

            let grammar =
                    [ 
                        syntaxTypeClause "Foo" ["x","x"]
                            [
                                rule "foo" [l (slex "foo")]
                            ]
                     ]
                
                errs = errors (snd (runElaborator (elabGrammar grammar)))

            shouldSatisfy errs (any hasRepeatedVariables)
        
        it "succeeds on unique variables" $ do

            let grammar =
                    [ 
                        syntaxTypeClause "Foo" ["x","y"]
                            [
                                rule "foo" [l (slex "foo")]
                            ]
                     ]
                
                errs = errors (snd (runElaborator (elabGrammar grammar)))

            shouldNotSatisfy errs (any hasRepeatedVariables)
        
        it "errors on unbound variables in Syntax Type Clause `Foo x, y ::= z (foo)`" $ do

            let grammar =
                    [ 
                        syntaxTypeClause "Foo" ["x","y"]
                            [
                                rule "foo" [v "z"]
                            ]
                     ]
                
                errs = errors (snd (runElaborator (elabGrammar grammar)))

            shouldSatisfy errs (any hasUnboundVariables)



    describe "The CFG representation" $ do

        {-
            Expr -> literal
            Expr -> sum
            literal -> Nat
            sum -> Expr + Expr
            Nat -> zero
            Nat -> successor
            zero -> 0
            successor -> s Nat
        -}

        let razorCFG =
                CFG [
                    Rule (NonTerminal "Expr")
                        [NonTerminalPart (NonTerminal "literal")]
                ,   Rule (NonTerminal "Expr")
                        [NonTerminalPart (NonTerminal "sum")]
                ,   Rule (NonTerminal "literal")
                        [NonTerminalPart (NonTerminal "Nat")]
                ,   Rule (NonTerminal "sum")
                        [NonTerminalPart (NonTerminal "Expr")
                        ,TerminalPart (StringTerminal "+")
                        ,NonTerminalPart (NonTerminal "Expr")]
                ,   Rule (NonTerminal "Nat")
                        [NonTerminalPart (NonTerminal "zero")]
                ,   Rule (NonTerminal "Nat")
                        [NonTerminalPart (NonTerminal "successor")]
                ,   Rule (NonTerminal "zero")
                        [TerminalPart (StringTerminal "0")]
                ,   Rule (NonTerminal "successor")
                        [TerminalPart (StringTerminal "s")
                        ,NonTerminalPart (NonTerminal "Nat")]
                 ]

        it "produces correct CFGs for Hutton's Razor" $ do

            {-
            
                Expr M, N
                    ::= n         (literal)
                      | M "+" N   (sum)
                      ;
                
                Nat m, n
                    ::= "0"     (zero)
                      | "s" n   (successor)
                      ;

            -}

            let razor =
                    [
                        syntaxTypeClause "Expr" ["M", "N"]
                            [ rule "literal" [v "n"]
                            , rule "sum" [v "M", l (slex "+"), v "N"]
                            ]
                    ,   syntaxTypeClause "Nat" ["m", "n"]
                            [ rule "zero" [l (slex "0")]
                            , rule "successor" [l (slex "s"), v "n"]
                            ]
                     ]
                    
                actual = grammarToCFG razor

            shouldBe actual razorCFG
            
        it "produces correct BNF strings" $ do

            {-
            
                <Expr>
                    ::= <literal>
                      | <sum>
                <literal>
                    ::= <Nat>
                <sum>
                    ::= <Expr> "+" <Expr>
                <Nat>
                    ::= <zero>
                      | <succesor>
                <zero>
                    ::= "0"
                <succesors>
                    ::= "s" <Nat>

            -}

            let razorBNF = unlines'
                    [ "<Expr>"
                    , "  ::= <literal>"
                    , "    | <sum>"
                    , "<literal>"
                    , "  ::= <Nat>"
                    , "<sum>"
                    , "  ::= <Expr> \"+\" <Expr>"
                    , "<Nat>"
                    , "  ::= <zero>"
                    , "    | <successor>"
                    , "<zero>"
                    , "  ::= \"0\""
                    , "<successor>"
                    , "  ::= \"s\" <Nat>"
                    ]
            
                actual = cfgToBNF razorCFG

            shouldBe actual razorBNF