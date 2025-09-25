{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Main.Utf8 qualified as Utf8
import Shh
import Shh.Nix (loadExeNix)
import System.Directory
import System.FilePath
import System.IO.Temp

-- Load executables with static Nix paths:
$(loadExeNix "cabal" "cabal")
$(loadExeNix "op" "op")

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    withSystemTempDirectory "hackage-publish" $ \tmpDir -> do
      putTextLn $ "Using temporary directory: " <> show tmpDir

      -- Run cabal sdist
      putTextLn "Creating source distribution..."
      cabal "sdist" "-o" tmpDir

      -- Get password from 1password
      putTextLn "Retrieving password from 1password..."
      void exitFailure -- TODO:
      password <- op "read" "op://Private/Hackage/password" |> captureTrim

      -- Find the tarball file
      files <- listDirectory tmpDir
      let maybeTarball = viaNonEmpty head $ filter (\f -> takeExtension f == ".gz") files
      case maybeTarball of
        Nothing -> error "No .gz tarball found in sdist output"
        Just tarball -> do
          -- Upload to hackage
          putTextLn "Publishing to Hackage..."
          cabal "upload" "--publish" "-u" "sridca" "-p" (decodeUtf8 @String password) (tmpDir </> tarball)

      putTextLn "Successfully published to Hackage!"
