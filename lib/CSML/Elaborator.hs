{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CSML.Elaborator where

import Control.Monad.State
import Data.List

import CSML.Grammar

newtype ElabState = ElabState { errors :: [ElabError] }

emptyElabState :: ElabState
emptyElabState = ElabState { errors = [] }

data ElabError
    = RepeatedVariable VariableName [RepeatedVariableLocation]
    | UnboundVariable VariableName UnboundVariableLocation
    deriving (Show)

data RepeatedVariableLocation
    = InSyntaxTypeClause Clause
    | InLexemeSynonymClause Clause
    deriving (Show, Eq)

data UnboundVariableLocation
    = InRule Clause Int -- Int = rule index
    deriving (Show)

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
collectBindingVariablesClause clause@(SyntaxTypeClause _ vns _) =
    [ (vn, InSyntaxTypeClause clause) | vn <- vns ]
collectBindingVariablesClause clause@(LexemeSynonymClause vn _) =
    [(vn, InLexemeSynonymClause clause)]


elabClause :: Context -> Clause -> Elaborator ()
elabClause ctx clause@(SyntaxTypeClause _ _ rs) =
    zipWithM_ (elabRule ctx clause) [0..] rs
elabClause _ (LexemeSynonymClause _ _) = return ()

elabRule :: Context -> Clause -> Int -> Rule -> Elaborator ()
elabRule ctx clause i (Rule _ parts) = mapM_ (elabRulePart ctx clause i) parts

elabRulePart :: Context -> Clause -> Int -> RulePart -> Elaborator ()
elabRulePart ctx clause i (VariablePart vn) =
    if vn `elem` ctx
        then return ()
        else addError $ UnboundVariable vn (InRule clause i)
elabRulePart _ _ _ (LexemePart _) = return ()