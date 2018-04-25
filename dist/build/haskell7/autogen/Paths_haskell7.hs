{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell7 (
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

bindir     = "/Users/Saksham_mittal/Documents/haskell-7/.cabal-sandbox/bin"
libdir     = "/Users/Saksham_mittal/Documents/haskell-7/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2/haskell7-0.1.0.0-3plJOAzhRoMnBjUMpckTY-haskell7"
dynlibdir  = "/Users/Saksham_mittal/Documents/haskell-7/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/Saksham_mittal/Documents/haskell-7/.cabal-sandbox/share/x86_64-osx-ghc-8.2.2/haskell7-0.1.0.0"
libexecdir = "/Users/Saksham_mittal/Documents/haskell-7/.cabal-sandbox/libexec/x86_64-osx-ghc-8.2.2/haskell7-0.1.0.0"
sysconfdir = "/Users/Saksham_mittal/Documents/haskell-7/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell7_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell7_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell7_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell7_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell7_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell7_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
