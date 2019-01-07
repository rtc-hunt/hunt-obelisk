{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts, OverloadedLists #-}
module Zoomeval.Shared where
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
  toHtml (EvalResult _ a _) = wrapWithPage $ do
        div_ $ toHtml a

instance ToHtml TypeResult where
  toHtml (TypeResult _ a) = wrapWithPage $ do
        div_ $ toHtml a

