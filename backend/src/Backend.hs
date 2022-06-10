{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Snap.Core
import Common.Route
import Obelisk.Backend
import Obelisk.Configs
import Control.Monad.IO.Class

import Data.Map
import Data.Text
import Data.ByteString
import qualified Obelisk.ExecutableConfig.Lookup as Cfg
--import Obelisk.Configs

import Data.Aeson
import qualified Control.Monad.Fail as Fail
import Data.Text as T
import Data.ByteString as BS
import Data.Map as Map
import System.Directory
import System.FilePath.Posix
import Control.Monad

-- | Get and parse a json configuration
getJsonConfigBase :: (HasConfigs m, FromJSON a) => Text -> m (Maybe (Either String a))
getJsonConfigBase = (fmap.fmap) (eitherDecodeStrict') . getConfig

-- | Get and parse a json configuration
getJsonConfig :: (HasConfigs m, FromJSON a, Fail.MonadFail m) => Text -> m a
getJsonConfig k = getJsonConfigBase k >>= \case
  Nothing -> Fail.fail $ "getJsonConfig missing key: " <> T.unpack k
  Just (Left err) -> Fail.fail $ "getJsonConfig invalid for key " <> T.unpack k <> " : " <> err
  Just (Right val) -> pure val


getConfigsFromDirectory :: FilePath -> IO (Map Text ByteString)
getConfigsFromDirectory base = doesDirectoryExist base >>= \case
  True -> do
    ps <- listDirectory base
    fmap mconcat $ forM ps $ \p -> do
      subdirConfigs <- getConfigsFromDirectory $ base </> p
      pure $ Map.mapKeys (T.pack . (p </>) . T.unpack) subdirConfigs
  False -> doesFileExist base >>= \case
    True -> Map.singleton "" <$> BS.readFile base
    False -> pure mempty

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      _ -> do
        liftIO $ f =<< Prelude.readFile "config/common/fakeConfig" 
        x <- liftIO $ getConfigsFromDirectory "config/backend"
        liftIO $ print $ Map.lookup "fakeConfig" x
        return ()

        where
          f = \case
            "" -> print "FAILUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUURE"
            something -> print something 
    -- \serve -> Cfg.getConfigs >>= flip runConfigsT $ do
    --   configs <- getConfigs
    --   smtpConfig <- getJsonConfig "backend/smtp"
    --   --liftIO $ print smtpConfig
    --   pure ()
      

    -- \serve -> do
    --   --getConfigs
    --   serve $ \case
    --     _ -> do
    --       -- let
    --       --   f :: (a -> Snap b) -> Snap b
    --       --   f = undefined
          
    --       -- f $ \x -> do
    --       --   y <- getConfigs
    --       --   pure y

    --       let getConfigs' :: Maybe (Map Text ByteString)
    --           getConfigs' = getConfigs 
    --       pure ()
        
    --   --withBackendConfig $ \app -> do
    --   --_ <- liftIO $ runConfigsT getConfigs
      

  , _backend_routeEncoder = fullRouteEncoder
  }


-- withBackendConfig :: (CI -> IO a) -> IO a
-- withBackendConfig _ 














-- ob deploy init reads the .gitignore and then can know what the full picture of the repo is
-- between the remote and the private local repo

-- this matters since the local repo is the only one who knows the secrets of the server


-- But also, then theres a 3rd state:

--    what if they have a config item that only applies to the server?

--    not necessarily in terms of "shape" but values (this definitely applies) 



-- SO; REVISED:

--   ob deploy *init* just gets the value of config from the thunked directory and copies it into the config dir
--   simply out of ease for the user

  
