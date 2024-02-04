{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CSML.Elaborator where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.String.Utils (strip)

import CSML.Grammar
import CSML.TextTools

newtype ElabState = ElabState { errors :: [ElabError] }
    deriving (Show)

emptyElabState :: ElabState
emptyElabState = ElabState { errors = [] }





newtype Elaborator a = Elaborator { runElab :: State ElabState a }
    deriving (Functor, Applicative, Monad, MonadState ElabState)

runElaborator :: Elaborator a -> (a, ElabState)
runElaborator e = runState (runElab e) emptyElabState

addError :: ElabError -> Elaborator ()
addError e = modify $ \ s -> s { errors = e:errors s }

type Context = [VariableName]

elabGrammar :: Grammar -> Elaborator ()
elabGrammar g = do
    let varsAndLocs = collectBindingVariablesGrammar g
        vars = map fst varsAndLocs
        uniqVars = nub vars
        repeatedVars = vars \\ uniqVars
    
    unless (null repeatedVars) $
        forM_ repeatedVars $ \vn ->
            let locs = nub [ loc | (vn',loc) <- varsAndLocs, vn' == vn ]
            in addError $ RepeatedVariable vn locs

    mapM_ (elabClause uniqVars) g

collectBindingVariablesGrammar
    :: Grammar -> [(VariableName, RepeatedVariableLocation)]
collectBindingVariablesGrammar g = g >>= collectBindingVariablesClause

collectBindingVariablesClause
    :: Clause -> [(VariableName, RepeatedVariableLocation)]
collectBindingVariablesClause clause@(SyntaxTypeClause _ _ vns _) =
    [ (vn, clause) | vn <- vns ]
collectBindingVariablesClause clause@(LexemeSynonymClause _ vn _) =
    [(vn, clause)]


elabClause :: Context -> Clause -> Elaborator ()
elabClause ctx clause@(SyntaxTypeClause _ _ _ rs) =
    zipWithM_ (elabRule ctx clause) [0..] rs
elabClause _ (LexemeSynonymClause {}) = return ()

elabRule :: Context -> Clause -> Int -> Rule -> Elaborator ()
elabRule ctx clause i (Rule _ _ parts) = mapM_ (elabRulePart ctx clause i) parts

elabRulePart :: Context -> Clause -> Int -> RulePart -> Elaborator ()
elabRulePart ctx clause i (VariablePart vn) =
    if vn `elem` ctx
        then return ()
        else addError $ UnboundVariable vn (InRule clause i)
elabRulePart _ _ _ (LexemePart _) = return ()




data ElabError
    = RepeatedVariable VariableName [Clause]
    | UnboundVariable VariableName UnboundVariableLocation
    deriving (Show)

type RepeatedVariableLocation = Clause

data UnboundVariableLocation
    = InRule Clause Int -- Int = rule index
    deriving (Show)

prettyElabErrors :: [ElabError] -> String
prettyElabErrors errs = intercalate "\n" (errs >>= prettyElabError)

prettyElabError :: ElabError -> [String]
prettyElabError (RepeatedVariable (VariableName vn) locs) =
    ""
    : ("Repeated variable " ++ vn)
    : "In locations:"
    : ""
    : indent 2 (locs >>= prettyRepeatedVariableLocation)
prettyElabError (UnboundVariable (VariableName vn) loc) =
    ""
    : ("Unbound variable " ++ vn)
    : "In location:"
    : indent 2 (prettyUnboundVariableLocation loc)

prettyRepeatedVariableLocation :: RepeatedVariableLocation -> [String]
prettyRepeatedVariableLocation cls =
    maybe ["unknown-source"] (\x -> [strip x]) (getSourceText (getMetaClause cls))

prettyUnboundVariableLocation :: UnboundVariableLocation -> [String]
prettyUnboundVariableLocation (InRule (SyntaxTypeClause cm (TypeName tn) _ rs) i) =
    let Rule rm (RuleName rn) _ = rs !! i
        rsrc = strip (fromMaybe "unknown-source" (getSourceText rm))
        csrc = strip (fromMaybe "unknown-source" (getSourceText cm))
    in
        [ "Syntax type definition for " ++ tn
        , "In rule " ++ rn
        , ""
        , "  " ++ rsrc
        , "of clause"
        , ""
        , csrc
        ]
prettyUnboundVariableLocation _ = []