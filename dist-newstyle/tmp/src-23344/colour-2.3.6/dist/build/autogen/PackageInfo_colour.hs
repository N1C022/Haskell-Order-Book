{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_colour (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "colour"
version :: Version
version = Version [2,3,6] []

synopsis :: String
synopsis = "A model for human colour/color perception"
copyright :: String
copyright = ""
homepage :: String
homepage = "http://www.haskell.org/haskellwiki/Colour"
