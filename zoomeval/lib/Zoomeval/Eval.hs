{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts, OverloadedLists #-}
module Zoomeval.Eval where
import System.Environment
import System.Posix.Process
import Servant
import Servant.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (Status (Status))
import Control.Monad.IO.Class
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import System.IO.Unsafe
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import GHC hiding (runGhc)
import Outputable hiding (parens, (<>))
import Type
import Unify
import Data.Maybe
import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson
import Zoomeval.API
import Lucid
import Servant.Utils.StaticFiles
import Zoomeval.Shared

import Debug.Trace

qualifiedImports :: [(String, Maybe String)]
qualifiedImports = [
            ("Cipher",Just "Cipher")
  ]

defaultBaseImports :: [String]
defaultBaseImports = [
            "Control.Applicative"
            , "Control.Arrow"
            , "Control.Monad"
            , "Control.Monad.Cont"
            , "Control.Monad.Except"
            , "Control.Monad.Fix"
            , "Control.Monad.Identity"
            , "Control.Monad.RWS"
            , "Control.Monad.Reader"
            , "Control.Monad.State"
            , "Control.Monad.Writer"
            , "Data.Array"
            , "Data.Aeson"
            , "Data.Bits"
            , "Data.Bool"
            , "Data.Char"
            , "Data.Complex"
            , "Data.Dynamic"
            , "Data.Either"
            , "Data.Eq"
            , "Data.Fixed"
            , "Data.Function"
            , "Data.Graph"
            , "Data.Int"
            , "Data.Ix"
            , "Data.List"
            --, "ShowFun"
            , "Data.Maybe"
            , "Data.Monoid"
            , "Data.Ord"
            , "Data.Ratio"
            , "Data.Tree"
            , "Data.Tuple"
            , "Data.Typeable"
            , "Data.Word"
            --, "Debug.SimpleReflect"
            , "System.Random"
            --, "Test.QuickCheck"
            --, "Text.PrettyPrint.HughesPJ"
            --, "Text.Printf"
            , "Crossword"
            , "Anagram"
            , "StandardDictionaries"
            , "Dictionary"
            , "ConsumptiveMonad"
            , "Fancy"
            --, "Text.Regex.TDFA"
            --, "Lucid"
            --, "Lucid.Bootstrap"
            --, "Data.Astro.Types"
            --, "Data.Astro.Coordinate"
            --, "Data.Astro.Planet"
            --, "Data.Astro.Time.JulianDate"
            --, "Data.Astro.Effects"
            --, "Data.Astro.CelestialObject.RiseSet"
            , "Prelude"
        ]

formatError (WontCompile errs) = GhcErrors $ unlines $ errMsg <$> errs
formatError a = GhcErrors $ show a

globalLock = unsafePerformIO $ newTMVarIO ()

oneAtATime handler = do
  liftIO $ atomically $ takeTMVar globalLock
  res <- handler
  liftIO $ atomically $ putTMVar globalLock ()
  return res

{-bracket
  (liftIO $ atomically $ waitTSem sem)
  (const $ liftIO $ atomically $ signalTSem sem) 
  . const-}

interpreterSetup pkgs = do
                reset -- Add safe to below before deploying!
                let mkSearchPath path = pure $ searchPath := ( (path <>) <$> ["", "/hunttools", "/hunttools-dicts-if", "/packed-dawg-big"])
                ze_src <- liftIO $ fromMaybe [] . fmap mkSearchPath <$> lookupEnv "ZE_SRC_TOP"
                set $ [ languageExtensions := [] ] <> ze_src
                if null ze_src then pure () else liftIO (traceIO "Loading source mods") >> loadModules ["Crossword", "Anagram", "StandardDictionaries", "Cipher", "Dictionary", "ConsumptiveMonad", "Fancy"]
                setImportsQ pkgs


doEval :: MonadIO m => [(String, Maybe String)] -> String -> Maybe (Map Text Text) -> m EvalResult
doEval pkgs exp _binds = oneAtATime $ do
        eth <- liftIO $ runInterpreter $ do
                interpreterSetup pkgs
                liftIO $ traceIO "Entering interpret"
                rv <- interpret ("take 4096 $ show " ++ parens exp) (as :: String) -- Will want fiddled with when we get other result types.
                liftIO $ traceIO "Done interpret"
                isFancy <- typeChecks $ "fancyToFrontend " ++ parens exp
                fancyResult <- if isFancy then Just <$> interpret ("fancyToFrontend " ++ parens exp) (as :: Value) else pure Nothing
                return (rv, fancyResult)
        case eth of
                Right (r, fr) -> return $ EvalResult exp (Right r) fr
                Left err -> return $ EvalResult exp (Left $ formatError err) Nothing

getType pkgs exp = oneAtATime $ do
        eth <- liftIO $ runInterpreter $ do
                interpreterSetup pkgs
                typeOf exp
        case eth of
                Right r -> return $ TypeResult exp $ Right r
                Left err -> return $ TypeResult exp $ Left $ formatError err

-- Not working yet.
getCompletions pkgs exp = return "UNSUPPORTED" {- oneAtATime $ do
    do
        eth <- liftIO $ runInterpreter $ do
                set [ languageExtensions := [Safe] ]
                unsafeSetGhcOption "-fdefer-typed-holes"
                setImportsQ pkgs
                srcTyp <- runGhc $ exprType $ parens exp
                --let (constr, targetType) = splitForAllTys srcTyp
                --let (spoo, flub) = splitPiTysInvisible targetType
                --let (nood, spook) = splitPiTys flub
                --runGhc $ execStmt "let aStrLst = [\"fooble\"]" execOptions
                df <- runGhc getSessionDynFlags
                --exps <- runGhc $ do 
                --             names <- getNamesInScope
                --             infos <- sequence $ getInfo True <$> names
                --             return $ showSDoc df <$> ppr <$> getOccName <$> filter isFuncLike [a|Just (a,_,_,_) <- infos]
                --exps <- getModuleExports "Prelude"
                --types <- runGhc $ sequence [exprType ("(" ++ a ++ ")") | a <- exps]
                --let filteredTypes = filter (isJust . tcMatchTy (mkForAllTys spoo $ binderType $ head nood) . fst) $ types `zip` [a | a <- exps]
                --return $ show $ (showSDoc df $ ppr nood, showSDoc df $ ppr constr, showSDoc df $ ppr filteredTypes) -- [a | Fun a <- exps] `zip` [showSDoc df $ ppr $ t|t<-types])
                --return $ unlines $ [showSDoc df $ ppr nood] ++ [b 
                    -- ++ " " ++ (showSDoc df $ ppr a)
                    -- ++ (showSDoc df $ ppr $ tcMatchTy (binderType $ head nood) a)
                --    | (a,b) <- filteredTypes]
                return $ showSDoc df $ ppr $ srcTyp
        case eth of
                Right r -> return r
                Left err -> return $ show err
        where 
                isFuncLike (AnId _) = True
                isFuncLike (AConLike _) = True
                isFuncLike _ = False
                -}

evalServer pkgs =
        doEval pkgs
        :<|> flip (doEval pkgs) mempty . fromMaybe "undefined"
        :<|> getType pkgs
        :<|> getCompletions pkgs

evaluator port = do
        loadPackages <- fromMaybe defaultBaseImports <$> fmap words <$> lookupEnv "ZE_MODULES"
        let qPackages = flip zip (repeat Nothing) loadPackages ++ qualifiedImports
        traceIO $ "Evaluator running on " <> (show port) <> "\n"
        -- doEval qPackages "crossword onelook \"foo\"" mempty
        traceIO $ "Evaluator booted on " <> (show port) <> "\n"
        run port $ serve theAPI $ evalServer qPackages

