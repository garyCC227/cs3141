{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Ex02 (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/garycc227/code/cs3141/lab/Ex02/.stack-work/install/x86_64-linux/lts-10.3/8.2.2/bin"
libdir     = "/home/garycc227/code/cs3141/lab/Ex02/.stack-work/install/x86_64-linux/lts-10.3/8.2.2/lib/x86_64-linux-ghc-8.2.2/Ex02-1.0-LZhHLD7pjVX6lJ36hU0KQA-Ex02"
dynlibdir  = "/home/garycc227/code/cs3141/lab/Ex02/.stack-work/install/x86_64-linux/lts-10.3/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/garycc227/code/cs3141/lab/Ex02/.stack-work/install/x86_64-linux/lts-10.3/8.2.2/share/x86_64-linux-ghc-8.2.2/Ex02-1.0"
libexecdir = "/home/garycc227/code/cs3141/lab/Ex02/.stack-work/install/x86_64-linux/lts-10.3/8.2.2/libexec/x86_64-linux-ghc-8.2.2/Ex02-1.0"
sysconfdir = "/home/garycc227/code/cs3141/lab/Ex02/.stack-work/install/x86_64-linux/lts-10.3/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Ex02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Ex02_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Ex02_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Ex02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Ex02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Ex02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
