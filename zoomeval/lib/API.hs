{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module API where

import Servant.API
import Data.Proxy
import Servant.HTML.Lucid
import Data.Aeson
import GHC.Generics

data EvalResult = EvalResult String String
        deriving (Generic, Show)
data TypeResult = TypeResult String String
        deriving (Generic, Show)

instance ToJSON EvalResult
instance ToJSON TypeResult
instance FromJSON EvalResult
instance FromJSON TypeResult

type TheAPI = "noodle" :> Capture "zog" String :> Get '[JSON] String
          :<|> "eval" :> Capture "expr" String :> Get '[JSON, HTML] EvalResult
          :<|> "eval" :> QueryParam "expr" String :> Get '[JSON, HTML] EvalResult
          :<|> "getType" :> Capture "expr" String :> Get '[JSON, HTML] TypeResult
          :<|> "getCompletions" :> Capture "expr" String :> Get '[JSON] String

theAPI :: Proxy TheAPI
theAPI = Proxy


