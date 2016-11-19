{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Control.Exception
import Data.Maybe
import Data.List (isPrefixOf, sort)
import Data.String

import qualified DMenu
import qualified Network.MPD as MPD

getTracks :: MonadIO m => m (Either MPD.MPDError [MPD.Song])
getTracks = liftIO $ do
  eLsInfos ← MPD.withMPD $ MPD.listAllInfo $ fromString "DnB" -- "" should be library path root ...
  pure $ fmap (>>= (\i → [ s | MPD.LsSong s ← [i] ])) eLsInfos
printTracks :: MonadIO m => m ()
printTracks = getTracks >>= \case
  Left err → liftIO $ putStrLn $ "Error: " ++ show err
  Right songs → liftIO $ print $ take 10 $ songs

main :: IO ()
main = getArgs >>= \case
  _ → do
    putStrLn usage
    exitFailure

usage :: String
usage = unlines
  [ "Usage: dmenu-mpd"
  ]
