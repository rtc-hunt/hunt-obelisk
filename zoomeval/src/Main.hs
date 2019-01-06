{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts, OverloadedLists #-}
module Main where
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

import Zoomeval.RoundRobin
import Zoomeval.Eval

main :: IO ()
main = do
        args <- getArgs
        if null args 
         then rootServerMain 8087
         else if (head args /= "evaluator")
           then rootServerMain $ read $ head args
           else evaluator $ read (args!!1)
