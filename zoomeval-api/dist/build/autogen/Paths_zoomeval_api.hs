{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_zoomeval_api (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jonored/.cabal/bin"
libdir     = "/home/jonored/.cabal/lib/x86_64-linux-ghc-8.4.4/zoomeval-api-0.1.0.0-LBxHrQHiI6Qvh6YkMZIwM"
dynlibdir  = "/home/jonored/.cabal/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/jonored/.cabal/share/x86_64-linux-ghc-8.4.4/zoomeval-api-0.1.0.0"
libexecdir = "/home/jonored/.cabal/libexec/x86_64-linux-ghc-8.4.4/zoomeval-api-0.1.0.0"
sysconfdir = "/home/jonored/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "zoomeval_api_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "zoomeval_api_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "zoomeval_api_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "zoomeval_api_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "zoomeval_api_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "zoomeval_api_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
