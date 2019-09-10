{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Scotty

import Control.Monad.IO.Class

import Data.FileEmbed
import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, replace)

import Lens.Micro

import Network.HTTP.Types.Status

import System.Directory

zones = 2

doc = $(embedStringFile "index.html")

main = scotty 3000 $ do
  get "/" $ render doc >>= html

  post "/toggle/0" $ toggle 0 >> goHome
  post "/toggle/1" $ toggle 1 >> goHome

toggle n = liftIO $ do
  state <- readState
  writeState $ over (ix n) (\s -> if s == '1' then '0' else '1') state

getStatePath :: IO FilePath
getStatePath =
  (++ "/.rpi-speaker-zones-state") <$> getHomeDirectory

readState :: IO String
readState = do
  statePath <- getStatePath
  exists <- doesFileExist statePath
  raw <- if exists then readFileStrict statePath
            else pure ""
  pure $ take zones $ raw ++ repeat '0'
    where
      readFileStrict path = do
        s <- readFile path
        length s `seq` pure s

writeState :: String -> IO ()
writeState contents = do
  statePath <- getStatePath
  writeFile statePath contents

render doc = do
  state <- liftIO readState
  pure $ replace "{state}" (pack state) doc

goHome = do
  status status302
  redirect "/"
