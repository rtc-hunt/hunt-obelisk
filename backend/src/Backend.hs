{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.ByteString as BS
import Data.Text.Encoding
import qualified Data.Text as T
import System.Environment
import System.Posix.Process
import Network.HTTP.Client (newManager, defaultManagerSettings, Proxy(..), httpLbs, parseRequest)
import qualified Network.HTTP.Client
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Servant.Client
import "servant-snap" Servant.Server.Internal
import Control.Monad.Error
import Control.Monad.Except
import Debug.Trace


{-
import System.Environment
import System.Posix.Process
import Servant
import Servant.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (Status (Status))
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import System.IO.Unsafe
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import GHC hiding (runGhc)
import Outputable hiding (parens)
import Type
import Unify
import Data.Maybe
import Data.Text (Text)
import Data.Map (Map)
import Zoomeval.API
import Lucid
import Servant.Utils.StaticFiles
-}

import Zoomeval.Eval
import Zoomeval.RoundRobin
import Zoomeval.API
import "servant-snap" Servant.Server (serveSnap, Context(..))
import Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = backendGo -- fmap (fromMaybe serveBackendEval) $ lookupEnv "ZE" >>= fmap serveBackend
  , _backend_routeEncoder = fullRouteEncoder
  }

backendGo serve = do
  zeExe <- lookupEnv "ZE"
  case zeExe of
    Nothing -> serveBackendEval serve
    Just path -> serveBackend path serve

serveBackendEval serve =
  serve $ const $ serveSnap theAPI $ evalServer $ flip zip (repeat Nothing) defaultBaseImports ++ qualifiedImports

zroth serve = serve $ const $
  do
    sub@(ClientEnv mgr base _) <- liftIO $ atomically $ readTQueue pool
    req <- getRequest
    outReq <- liftIO $ parseRequest $ (showBaseUrl base) <> (T.unpack $ decodeUtf8 $ rqURI req)
    rv <- liftIO $ httpLbs outReq mgr
    writeLBS $ Network.HTTP.Client.responseBody $ rv
    liftIO $ atomically $ writeTQueue pool sub
    

serveBackend zeExeName serve = do
  subPid <- sequence [forkProcess $ executeFile zeExeName False ["evaluator", show port] Nothing | port <- childPorts]
  manager <- newManager defaultManagerSettings
  sequence_ [atomically $ writeTQueue pool (ClientEnv manager (BaseUrl Http "localhost" port "")) | port <- childPorts]
  traceIO "Running server."
  zroth serve
 where
   myPort = 8010
   childPorts = [(myPort+1) .. (myPort + 29)]

{-


wrapWithPage :: Monad m => HtmlT m () -> HtmlT m ()
wrapWithPage html = 
  html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css", integrity_ "sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy", crossorigin_ "anonymous"]
    body_ $ do
      h1_ "Huntbot Repl v2"
      html
      form_ [action_ "", method_ "get"] $
        input_ [name_ "expr"]

instance ToHtml (Either GhcErrors String) where
  toHtml (Left (GhcErrors a)) = div_ $ toHtml a
  toHtml (Right a) = div_ $ toHtml a

instance ToHtml EvalResult where
  toHtml (EvalResult _ a) = wrapWithPage $ do
        div_ $ toHtml a

instance ToHtml TypeResult where
  toHtml (TypeResult _ a) = wrapWithPage $ do
        div_ $ toHtml a
qualifiedImports = [
            ("Cipher",Just "Cipher")
  ]

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
            , "ShowFun"
            , "Data.Maybe"
            , "Data.Monoid"
            , "Data.Ord"
            , "Data.Ratio"
            , "Data.Tree"
            , "Data.Tuple"
            , "Data.Typeable"
            , "Data.Word"
            , "Debug.SimpleReflect"
            , "System.Random"
            , "Test.QuickCheck"
            , "Text.PrettyPrint.HughesPJ"
            , "Text.Printf"
            , "Crossword"
            , "Anagram"
            , "StandardDictionaries"
            , "Dictionary"
            , "ConsumptiveMonad"
            , "Text.Regex.TDFA"
            , "Lucid"
            , "Lucid.Bootstrap"
            , "Data.Astro.Types"
            , "Data.Astro.Coordinate"
            , "Data.Astro.Planet"
            , "Data.Astro.Time.JulianDate"
            , "Data.Astro.Effects"
            , "Data.Astro.CelestialObject.RiseSet"
            , "Prelude"
        ]

formatError (WontCompile errs) = GhcErrors $ unlines $ errMsg <$> errs
formatError a = GhcErrors $ show a

doEval :: MonadIO m => [(String, Maybe String)] -> String -> Maybe (Map Text Text) -> m EvalResult
doEval pkgs exp _binds = do
        eth <- liftIO $ runInterpreter $ do
                set [ languageExtensions := [Safe] ]
                setImportsQ pkgs
                interpret ("take 4096 $ show " ++ parens exp) (as :: String) -- Will want fiddled with when we get other result types.
        case eth of
                Right r -> return $ EvalResult exp (Right r)
                Left err -> return $ EvalResult exp $ Left $ formatError err

getType pkgs exp = do
        eth <- liftIO $ runInterpreter $ do
                set [ languageExtensions := [Safe] ]
                setImportsQ pkgs
                typeOf exp
        case eth of
                Right r -> return $ TypeResult exp $ Right r
                Left err -> return $ TypeResult exp $ Left $ formatError err

-- Not working yet.
getCompletions pkgs exp = return "UNSUPPORTED" {- do
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
evalServer i pkgs =
        doEval pkgs
        :<|> flip (doEval pkgs) mempty . fromMaybe "undefined"
        :<|> getType pkgs
        :<|> getCompletions pkgs
-}
