{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_lasthomework (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "E:\\git\\haskellnew\\lasthomework\\.stack-work\\install\\3122d1ef\\bin"
libdir     = "E:\\git\\haskellnew\\lasthomework\\.stack-work\\install\\3122d1ef\\lib\\x86_64-windows-ghc-8.0.1\\lasthomework-0.1.0.0-38Oy90SkoIZ9X1g2jYCTPk"
datadir    = "E:\\git\\haskellnew\\lasthomework\\.stack-work\\install\\3122d1ef\\share\\x86_64-windows-ghc-8.0.1\\lasthomework-0.1.0.0"
libexecdir = "E:\\git\\haskellnew\\lasthomework\\.stack-work\\install\\3122d1ef\\libexec"
sysconfdir = "E:\\git\\haskellnew\\lasthomework\\.stack-work\\install\\3122d1ef\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lasthomework_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lasthomework_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lasthomework_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lasthomework_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lasthomework_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
