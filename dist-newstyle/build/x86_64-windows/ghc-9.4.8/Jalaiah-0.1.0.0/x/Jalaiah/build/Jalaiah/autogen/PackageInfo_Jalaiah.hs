{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Jalaiah (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Jalaiah"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Tracks the energy consumption for the given data"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
