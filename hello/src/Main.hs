{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe (fromJust)
import qualified Network.Wreq as W

import GHC.Generics
import Data.Text as T
import Data.Aeson (Result, FromJSON, fromJSON, Value)
import Data.Aeson.Lens (key, _Array)
import Control.Lens ( (^?), (^.) )
import Data.Vector
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as MR

data Post = P { author :: T.Text,
                title :: T.Text
              } deriving (Show, Generic, FromJSON)

class Monad m => MonadLog m where
  progress :: String -> m ()

instance MonadLog IO where
  progress p = putStrLn p

instance MonadLog m => MonadLog (MR.ReaderT c m) where
  progress p = MR.lift $ progress p 


main :: IO ()
main = liftIO $ (flip MR.runReaderT) ("HI" :: String) $ MR.runReaderT p (7 :: Integer)
 where p = do
             progress "REDDIT POST GETTER"
             response <- getRawPosts
             posts <- convertPostsToData response
             printPosts posts
             progress "The End"

getRawPosts :: (MonadLog m, MonadIO m) => m (W.Response BS.ByteString)
getRawPosts = do
  progress "About to request"
  liftIO $ W.get "http://www.reddit.com/r/haskell.json"

-- convertPostsToData :: (W.Response BS.ByteString) -> IO (Vector (Result Post))
convertPostsToData :: (MonadLog m) => (W.Response BS.ByteString) -> m (Vector (Result Post))
convertPostsToData r = do
  let posts = r ^. W.responseBody . key "data" . key "children" . _Array
  progress "Converted posts to vector"
  let post' :: Vector (Result Post) = fmap (fromJSON . unwrap) posts
  return post'

printPosts :: (MonadLog m, MonadIO m) => Vector (Result Post) -> m ()
printPosts ps = do
  progress "HELLO"
  liftIO $ print ps

unwrap :: Value -> Value
unwrap v = fromJust (v ^? key "data")

