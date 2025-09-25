{-# LANGUAGE TemplateHaskell #-}

module Shh.Nix (loadExeNix) where

import Language.Haskell.TH
import Shh (Cmd)
import System.Which (staticWhichNix)

-- Template Haskell function that combines staticWhichNix with shh to create
-- a Cmd function using the executable's static Nix store path at compile time.
loadExeNix :: String -> String -> Q [Dec]
loadExeNix fnName exeName = do
  pathExp <- staticWhichNix exeName
  case pathExp of
    LitE (StringL nixStorePath) -> do
      let name = mkName fnName
          impl =
            valD
              (varP name)
              ( normalB
                  [|
                    withFrozenCallStack $ exe ($(litE (stringL nixStorePath)) :: String)
                    |]
              )
              []
          typ = SigD name (ConT ''Cmd)
      i <- impl
      return [typ, i]
    _ -> fail $ "staticWhichNix didn't return a string literal for " ++ exeName
