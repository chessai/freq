{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_freq (
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

bindir     = "/home/chessai/.cabal/bin"
libdir     = "/home/chessai/.cabal/lib/x86_64-linux-ghc-8.2.2/freq-0.1.0.0-Eze87nWLojB8HZcm6xEtbi-freq"
dynlibdir  = "/home/chessai/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/chessai/.cabal/share/x86_64-linux-ghc-8.2.2/freq-0.1.0.0"
libexecdir = "/home/chessai/.cabal/libexec/x86_64-linux-ghc-8.2.2/freq-0.1.0.0"
sysconfdir = "/home/chessai/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "freq_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "freq_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "freq_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "freq_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "freq_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "freq_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
