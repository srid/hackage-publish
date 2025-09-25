{-# LANGUAGE TemplateHaskell #-}

module Main where

import Main.Utf8 qualified as Utf8
import Shh
import Shh.Nix
import System.Directory
import System.FilePath
import System.IO.Temp

-- Load executables with static Nix paths:
$(loadExeNix "cabal" "cabal")
$(loadExeNix "op" "op")

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    withSystemTempDirectory "hackage-publish" $ \tmpDir -> do
      putTextLn $ "Using temporary directory: " <> show tmpDir

      -- Run cabal sdist
      putTextLn "Creating source distribution..."
      cabal ("sdist" :: String) ("-o" :: String) tmpDir

      -- Get password from 1password
      putTextLn "Retrieving password from 1password..."
      password <- op ("read" :: String) ("op://Private/Hackage/password" :: String) |> captureTrim

      -- Find the tarball file
      files <- listDirectory tmpDir
      let maybeTarball = viaNonEmpty head $ filter (\f -> takeExtension f == ".gz") files
      case maybeTarball of
        Nothing -> error "No .gz tarball found in sdist output"
        Just tarball -> do
          -- Upload to hackage
          putTextLn "Publishing to Hackage..."
          cabal ("upload" :: String) ("--publish" :: String) ("-u" :: String) ("sridca" :: String) ("-p" :: String) (toString (decodeUtf8 password :: Text)) (tmpDir </> tarball)

      putTextLn "Successfully published to Hackage!"
