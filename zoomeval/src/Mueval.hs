{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeOperators, FlexibleContexts, RecordWildCards #-}
module Main where

import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (Status (Status))
import Zoomeval.API
import Servant.API hiding (header)
import Options.Applicative
import Data.Semigroup hiding (option)

data Args = Args {
        expr :: String,
        timeout :: Int,
        trusted :: [String],
        exts :: [String],
        imports :: Bool,
        load :: String
        }

args :: Parser Args
args = Args <$> 
        strOption ( long "expression" <> short 'e' <> metavar "EXPR" <> help "Expression to evaluate" )
        <*> option auto ( short 't' <> value 5 )
        <*> many (strOption (short 's'))
        <*> many (strOption (short 'X'))
        <*> switch ( long "no-imports" )
        <*> strOption (short 'l' <> value "")
eval :<|> _ :<|> _ = client theAPI
main :: IO ()
main = do
        Args {..} <- execParser (info (args <**> helper)
                        (fullDesc
                         <> progDesc "Mueval shim for zoomeval"
                         <> header "zoomueval - a shim for mueval compat for zoomeval"))
        --putStrLn expr
        manager <- newManager defaultManagerSettings
        (Right (EvalResult _ rv)) <- runClientM (eval expr Nothing) (ClientEnv manager (BaseUrl Http "localhost" 8087 "") Nothing)
        putStrLn $ show rv
