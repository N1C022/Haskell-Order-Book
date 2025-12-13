{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_prettyprinter (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "prettyprinter"
version :: Version
version = Version [1,7,1] []

synopsis :: String
synopsis = "A modern, easy to use, well-documented, extensible pretty-printer."
copyright :: String
copyright = ""
homepage :: String
homepage = "http://github.com/quchen/prettyprinter"
