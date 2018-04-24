module Paths_haskell7 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/om/haskell-7/.cabal-sandbox/bin"
libdir     = "/home/om/haskell-7/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/haskell7-0.1.0.0-GIhN5RJ5M3M7DXHrV56M4K"
datadir    = "/home/om/haskell-7/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/haskell7-0.1.0.0"
libexecdir = "/home/om/haskell-7/.cabal-sandbox/libexec"
sysconfdir = "/home/om/haskell-7/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell7_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell7_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell7_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell7_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell7_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
