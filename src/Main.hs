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

{- | Get the 1Password CLI binary to use at runtime.
Prefers /usr/bin/op if it exists, as the Nix package (pkgs._1password-cli)
may not always work correctly on non-NixOS systems.
-}
opBin :: IO (String -> String -> Proc ())
opBin = do
  usrBinOpExists <- doesFileExist "/usr/bin/op"
  pure $
    if usrBinOpExists
      then exe "/usr/bin/op"
      else op

opRead :: String -> String -> String -> IO String
opRead vault item field = do
  let uri = "op://" <> vault <> "/" <> item <> "/" <> field
  opCmd <- opBin
  result <- opCmd "read" uri |> captureTrim
  pure $ decodeUtf8 @String result

cabalSdist :: (HasCallStack) => FilePath -> IO FilePath
cabalSdist dir = do
  cabal "sdist" "-o" dir
  getFileMatching dir (\f -> takeExtension f == ".gz")

cabalHaddock :: (HasCallStack) => FilePath -> IO FilePath
cabalHaddock dir = do
  cabal "haddock" "--builddir" dir "--haddock-for-hackage"
  getFileMatching dir ("-docs.tar.gz" `isSuffixOf`)

getFileMatching :: FilePath -> (FilePath -> Bool) -> IO FilePath
getFileMatching dir predicate = do
  files <- listDirectory dir
  let maybeFile = viaNonEmpty head $ filter predicate files
  case maybeFile of
    Nothing -> error $ "No matching file found in " <> toText dir
    Just file -> pure $ dir </> file
