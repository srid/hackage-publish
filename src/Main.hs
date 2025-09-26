{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.List (isSuffixOf)
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
      -- Run cabal sdist
      putTextLn "🌀 Creating source distribution..."
      src <- cabalSdist tmpDir
      -- Run cabal haddock
      putTextLn "🌀 Creating haddock documentation..."
      haddock <- cabalHaddock tmpDir

      -- Get password from 1password
      putTextLn "🌀 Retrieving password from 1password..."
      username <- opRead "Private" "Hackage" "username"
      password <- opRead "Private" "Hackage" "password"

      -- Upload to hackage
      putTextLn $ "🌀 Publishing sdist '" <> toText (takeFileName src) <> "' to Hackage as " <> toText username <> "..."
      cabal "upload" "--publish" "-u" username "-p" password (tmpDir </> src)
      putTextLn $ "🌀 Publishing haddock '" <> toText (takeFileName haddock) <> "' to Hackage as " <> toText username <> "..."
      cabal "upload" "--publish" "-u" username "-p" password "-d" (tmpDir </> haddock)

      putTextLn "✅ Successfully published to Hackage!"

opRead :: String -> String -> String -> IO String
opRead vault item field = do
  let uri = "op://" <> vault <> "/" <> item <> "/" <> field
  result <- op "read" uri |> captureTrim
  pure $ decodeUtf8 @String result

cabalSdist :: (HasCallStack) => FilePath -> IO FilePath
cabalSdist dir = do
  cabal "sdist" "-o" dir
  files <- listDirectory dir
  let maybeTarball = viaNonEmpty head $ filter (\f -> takeExtension f == ".gz") files
  case maybeTarball of
    Nothing -> error "No .gz tarball found in sdist output"
    Just tarball -> pure $ dir </> tarball

cabalHaddock :: (HasCallStack) => FilePath -> IO FilePath
cabalHaddock dir = do
  cabal "haddock" "--builddir" dir "--haddock-for-hackage"
  files <- listDirectory dir
  let maybeHaddock = viaNonEmpty head $ filter ("-docs.tar.gz" `isSuffixOf`) files
  case maybeHaddock of
    Nothing -> error "No doc .tar.gz found in haddock output"
    Just haddock -> pure $ dir </> haddock
