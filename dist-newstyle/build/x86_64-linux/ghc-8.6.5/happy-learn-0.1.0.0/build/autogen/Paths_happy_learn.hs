{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_happy_learn (
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

bindir     = "/home/jimmy/.cabal/bin"
libdir     = "/home/jimmy/.cabal/lib/x86_64-linux-ghc-8.6.5/happy-learn-0.1.0.0-inplace"
dynlibdir  = "/home/jimmy/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/jimmy/.cabal/share/x86_64-linux-ghc-8.6.5/happy-learn-0.1.0.0"
libexecdir = "/home/jimmy/.cabal/libexec/x86_64-linux-ghc-8.6.5/happy-learn-0.1.0.0"
sysconfdir = "/home/jimmy/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "happy_learn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "happy_learn_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "happy_learn_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "happy_learn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "happy_learn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "happy_learn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
