{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}


module Backend where

import Snap.Core
import Common.Route
import Obelisk.Backend
import Obelisk.Configs
import Control.Monad.IO.Class

import Data.Map
import Data.Text
import Data.ByteString

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> Cfg.getConfigs >>= flip runConfigsT do
      configs <- getConfigs
      smtpConfig <- getJsonConfig "backend/smtp"
      liftIO $ print smtpConfig 

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
