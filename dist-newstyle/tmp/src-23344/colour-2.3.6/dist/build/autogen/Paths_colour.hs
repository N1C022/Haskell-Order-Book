{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_colour (
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
version = Version [2,3,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.6.7\\colour-2.3.6-3d8866d212df099a11e4691917d6690bf0153649\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.6.7\\colour-2.3.6-3d8866d212df099a11e4691917d6690bf0153649\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.6.7\\colour-2.3.6-3d8866d212df099a11e4691917d6690bf0153649\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.6.7\\colour-2.3.6-3d8866d212df099a11e4691917d6690bf0153649\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.6.7\\colour-2.3.6-3d8866d212df099a11e4691917d6690bf0153649\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.6.7\\colour-2.3.6-3d8866d212df099a11e4691917d6690bf0153649\\etc"

getBinDir     = catchIO (getEnv "colour_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "colour_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "colour_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "colour_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "colour_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "colour_sysconfdir") (\_ -> return sysconfdir)



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
