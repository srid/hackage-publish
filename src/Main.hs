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
      -- Run cabal sdist
      putTextLn "🌀 Creating source distribution..."
      tarball <- cabalSdist tmpDir

      -- Get password from 1password
      putTextLn "🌀 Retrieving password from 1password..."
      username <- opRead "Private" "Hackage" "username"
      password <- opRead "Private" "Hackage" "password"

      -- Upload to hackage
      putTextLn $ "🌀 Publishing '" <> toText (takeFileName tarball) <> "' to Hackage as " <> toText username <> "..."
      cabal "upload" "--publish" "-u" username "-p" password (tmpDir </> tarball)

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
