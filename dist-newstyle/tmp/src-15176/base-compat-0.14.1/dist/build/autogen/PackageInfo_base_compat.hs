{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_base_compat (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "base_compat"
version :: Version
version = Version [0,14,1] []

synopsis :: String
synopsis = "A compatibility layer for base"
copyright :: String
copyright = "(c) 2012-2018 Simon Hengel,\n(c) 2014-2018 Jo\227o Crist\243v\227o,\n(c) 2015-2018 Ryan Scott"
homepage :: String
homepage = ""
