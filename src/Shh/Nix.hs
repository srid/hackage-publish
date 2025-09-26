-- | TODO: Upstream to a package.
module Shh.Nix (loadExeNix) where

import Language.Haskell.TH
import Shh.Internal (rawExe)
import System.Which (staticWhichNix)

-- Load an executable in PATH assuming it is in /nix/store. Uses staticWhichNix under the hood.
loadExeNix :: String -> String -> Q [Dec]
loadExeNix fnName exeName = do
  pathExp <- staticWhichNix exeName
  case pathExp of
    LitE (StringL nixStorePath) -> rawExe fnName nixStorePath
    _ -> fail $ "staticWhichNix didn't return a string literal for " ++ exeName
