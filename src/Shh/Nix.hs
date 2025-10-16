-- | TODO: Upstream to a package.
module Shh.Nix (loadExeNix) where

import Language.Haskell.TH
import Shh.Internal (rawExe)
import System.Directory (doesFileExist)
import System.Which (staticWhichNix)

-- Load an executable in PATH assuming it is in /nix/store. Uses staticWhichNix under the hood.
-- For 'op', it prefers /usr/bin/op if it exists (for non-NixOS compatibility).
loadExeNix :: String -> String -> Q [Dec]
loadExeNix fnName exeName = do
  -- Special case: prefer /usr/bin/op over nix package on non-NixOS systems
  if exeName == "op"
    then do
      usrBinExists <- runIO $ doesFileExist "/usr/bin/op"
      if usrBinExists
        then rawExe fnName "/usr/bin/op"
        else fallbackToNix
    else fallbackToNix
  where
    fallbackToNix = do
      pathExp <- staticWhichNix exeName
      case pathExp of
        LitE (StringL nixStorePath) -> rawExe fnName nixStorePath
        _ -> fail $ "staticWhichNix didn't return a string literal for " ++ exeName
