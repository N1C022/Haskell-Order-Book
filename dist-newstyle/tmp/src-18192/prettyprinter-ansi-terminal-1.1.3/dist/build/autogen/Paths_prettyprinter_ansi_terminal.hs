{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_prettyprinter_ansi_terminal (
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
version = Version [1,1,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.6.7\\prettyprinter_-1.1.3-d3ab101fdbf58f09719844ac6e318e638150dafb\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.6.7\\prettyprinter_-1.1.3-d3ab101fdbf58f09719844ac6e318e638150dafb\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.6.7\\prettyprinter_-1.1.3-d3ab101fdbf58f09719844ac6e318e638150dafb\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.6.7\\prettyprinter_-1.1.3-d3ab101fdbf58f09719844ac6e318e638150dafb\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.6.7\\prettyprinter_-1.1.3-d3ab101fdbf58f09719844ac6e318e638150dafb\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.6.7\\prettyprinter_-1.1.3-d3ab101fdbf58f09719844ac6e318e638150dafb\\etc"

getBinDir     = catchIO (getEnv "prettyprinter_ansi_terminal_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "prettyprinter_ansi_terminal_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "prettyprinter_ansi_terminal_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "prettyprinter_ansi_terminal_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prettyprinter_ansi_terminal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prettyprinter_ansi_terminal_sysconfdir") (\_ -> return sysconfdir)



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
