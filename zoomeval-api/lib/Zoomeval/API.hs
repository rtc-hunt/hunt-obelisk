{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Zoomeval.API where

import Servant.API
import qualified Data.Text as T
import Data.Proxy
import Servant.HTML.Lucid
import Data.Aeson
import Data.Text
import Data.Map (Map)
import Data.Bifunctor
import Text.Read
import GHC.Generics

data GhcErrors = GhcErrors String
        deriving (Generic, Show)

data EvalResult = EvalResult {
        evalExpr :: String,
        evalResult :: Either GhcErrors String,
        evalFancyResult :: Maybe Value
        }
        deriving (Generic, Show)

data TypeResult = TypeResult String (Either GhcErrors String)
        deriving (Generic, Show)

instance ToJSON GhcErrors
instance ToJSON EvalResult
instance ToJSON TypeResult
instance FromJSON GhcErrors
instance FromJSON EvalResult
instance FromJSON TypeResult

data QueryBindings

type TheAPI =  "eval" :> Capture "expr" String :> QueryParam "bindings" (Map Text Text) :> Get '[JSON, HTML] EvalResult
          :<|> "eval" :> QueryParam "expr" String :> Get '[JSON, HTML] EvalResult
          :<|> "getType" :> Capture "expr" String :> Get '[JSON, HTML] TypeResult
          :<|> "getCompletions" :> Capture "expr" String :> Get '[JSON] String

instance ToHttpApiData (Map Text Text) where
  toQueryParam = T.pack . show

instance FromHttpApiData (Map Text Text) where
  parseQueryParam = first T.pack . readEither . T.unpack

theAPI :: Proxy TheAPI
theAPI = Proxy


