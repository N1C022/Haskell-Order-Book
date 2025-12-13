{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_base_compat (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,14,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.6.7\\base-compat-0.14.1-bd8f2fdfb75279b26c68d3e2cf2843e319f6dc40\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.6.7\\base-compat-0.14.1-bd8f2fdfb75279b26c68d3e2cf2843e319f6dc40\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.6.7\\base-compat-0.14.1-bd8f2fdfb75279b26c68d3e2cf2843e319f6dc40\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.6.7\\base-compat-0.14.1-bd8f2fdfb75279b26c68d3e2cf2843e319f6dc40\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.6.7\\base-compat-0.14.1-bd8f2fdfb75279b26c68d3e2cf2843e319f6dc40\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.6.7\\base-compat-0.14.1-bd8f2fdfb75279b26c68d3e2cf2843e319f6dc40\\etc"

getBinDir     = catchIO (getEnv "base_compat_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "base_compat_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "base_compat_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "base_compat_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "base_compat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "base_compat_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
