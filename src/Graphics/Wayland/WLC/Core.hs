{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes, ImpredicativeTypes, KindSignatures #-}

module Graphics.Wayland.WLC.Core
  (
  ) where

import Foreign
import Foreign.C

import Graphics.Wayland.WLC.Internal.TH
import Graphics.Wayland.WLC.Types.Internal

import Control.Monad (join)
import Control.Applicative ((<*>))

foreign import ccall safe "wlc_init"      wlcInit      :: IO Bool
foreign import ccall safe "wlc_terminate" wlcTerminate :: IO ()
foreign import ccall safe "wlc_run"       wlcRun       :: IO ()

foreign import ccall safe "wlc_get_backend_type" getBackendType :: IO BackendType

foreign import ccall safe "wlc_handle_set_user_data" c_setHandleUserData :: Ptr () -> Ptr () -> IO ()
setHandleUserdata :: Handle h => h -> a -> IO (IO ())
setHandleUserdata h d = do
  h'  <- marshal h
  d'  <- newStablePtr d
  d'' <- marshal d'
  c_setHandleUserData h' d''
  return $ freeStablePtr d' -- return the finalizer

foreign import ccall safe "wlc_handle_set_user_data" c_getHandleUserData :: Ptr () -> IO (Ptr ())
getHandleUserdata :: Handle h => h -> IO a
getHandleUserdata h = do
  h' <- marshal h
  d' <- c_getHandleUserData h'
  d  <- unmarshal d'
  deRefStablePtr d

