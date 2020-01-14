{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts, OverloadedLists, RecordWildCards #-}
module Zoomeval.RoundRobin where
import System.Environment
import System.Posix.Process
import Servant
import Servant.Client
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (Status (..))
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
import Zoomeval.Shared
import Debug.Trace


-- Below this point is the fork-and-round-robin implementation. It should be pretty much general.
-- Also need some "resource use is getting too high in a worker, kill it and restart" here, but it's not present yet.
-- This does mean there's a denial-of-service attack here at least.

--servantErrorToErr (FailureResponse _ (Status code reason) bodyCT body) = ServantErr code (show reason) body []
servantErrorToErr (FailureResponse _ (Response {..})) = ServerError (statusCode responseStatusCode) (show $ statusMessage responseStatusCode) responseBody []

unRunHandler :: IO (Either ClientError a) -> Handler a
unRunHandler op = do
        res <- liftIO op
        case res of Left a -> throwError $ servantErrorToErr a
                    Right a -> return a

withClient :: ClientM a -> Handler a
withClient func = unRunHandler $ do
        ce <- liftIO $ atomically $ readTQueue pool
        traceIO "Got a worker"
        res <- runClientM func ce
        traceIO "Finished with worker"
        liftIO $ atomically $ writeTQueue pool ce
        return res

-- proxyOp :: (ClientM :~> Handler)
-- proxyOp = NT withClient

rootProxy :: Server TheAPI
--rootProxy = enter proxyOp $ client theAPI
rootProxy = hoistServer theAPI withClient $ client theAPI

pool = unsafePerformIO $ newTQueueIO

rootServerMain myPort = do
        path <- getExecutablePath
        subPid <- sequence [forkProcess $ executeFile path False ["evaluator", show port] Nothing | port <- childPorts]
        manager <- newManager defaultManagerSettings
        --pool <- atomically $ newTQueue
        sequence_ [atomically $ writeTQueue pool (ClientEnv manager (BaseUrl Http "localhost" port "")) | port <- childPorts]

        --zeSiteMay <- lookupEnv "ZE_SITE"
        --let zeSite = fromMaybe "" zeSiteMay
        --let webApp = serveDirectoryFileServer zeSite
        traceIO "Running server."
        run myPort $ serve (Proxy :: Proxy (TheAPI)) $ rootProxy -- :<|> webApp
       where
        childPorts = [(myPort+1) .. (myPort + 9)]

--webApp = serveDirectoryFileServer "/home/jonored/huntstuff/zoomeval/frontend/dist/build/zoomeval-frontend/zoomeval-frontend.jsexe/"

