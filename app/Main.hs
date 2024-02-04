module Main where

import Control.Monad
import Options.Applicative
import System.Directory

import qualified CSML.CSMLParser as CSML
import qualified CSML.Elaborator as CSML

main :: IO ()
main = execParser opts >>= runCommand
  where
    opts = info (cmd <**> helper)
                ( fullDesc
                  <> header "pltk - a programming language toolkit" )

data Command
  = Check { grammarPath :: FilePath }
  | Parse { sourcePath :: FilePath, grammarPath :: FilePath }

cmd :: Parser Command
cmd = hsubparser (
    command "check" (info check (progDesc "Check if a grammar is valid"))
      <>
    command "parse" (info parse (progDesc "Parse source code with a grammar"))
  )

  where

    check :: Parser Command
    check = Check <$> checkGrammarPath
      where
        checkGrammarPath :: Parser FilePath
        checkGrammarPath = argument str (metavar "GRAMMARPATH")

    parse :: Parser Command
    parse = Parse <$> parseSourcePath <*> parseGrammarPath
      where
        parseSourcePath :: Parser FilePath
        parseSourcePath = argument str (metavar "SOURCEPATH")

        parseGrammarPath :: Parser FilePath
        parseGrammarPath = argument str (metavar "GRAMMARPATH")

runCommand :: Command -> IO ()
runCommand (Check grm) = runCheck grm
runCommand (Parse src grm) = runParse src grm

runCheck :: FilePath -> IO ()
runCheck grm = do
  grmExists <- doesFileExist grm
  unless grmExists $ putStrLn ("No such grammar file: " ++ grm)
  when grmExists $ do
    grmSrc <- readFile grm
    case CSML.parseSource grm grmSrc of
      Left err -> do
        putStrLn "Grammar parse: Failed"
        putStrLn ""
        putStrLn err
      Right g -> do
        putStrLn "Grammar parse: Succeeded"
        case CSML.runElaborator (CSML.elabGrammar g) of
          (_, CSML.ElabState []) -> putStrLn "Check grammar: Succeeded"
          (_, CSML.ElabState errs) -> do
            putStrLn "Check grammar: Failed"
            putStrLn (CSML.prettyElabErrors errs)

runParse :: FilePath -> FilePath -> IO ()
runParse src grm = do
  srcExists <- doesFileExist src
  grmExists <- doesFileExist grm
  unless srcExists $ putStrLn ("No such source file: " ++ src)
  unless grmExists $ putStrLn ("No such grammar file: " ++ grm)
  when (srcExists && grmExists) $ do
    putStrLn $ "parsing " ++ src ++ " with " ++ grm