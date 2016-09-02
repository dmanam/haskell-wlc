{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Graphics.Wayland.WLC.Core
  ( wlcInit, wlcTerminate, wlcRun
  , getBackendType
  , setHandleUserdata, getHandleUserdata
  --, addFileEvent, addTimer, timerUpdate, removeEventSource
  ) where

import Foreign
import Foreign.C

import Graphics.Wayland.WLC.Internal.TH
import Graphics.Wayland.WLC.Types.Internal

import Control.Monad (join, (<=<))
import Control.Applicative ((<*>))

foreign import ccall safe "wlc_init"      wlcInit      :: IO Bool
foreign import ccall safe "wlc_terminate" wlcTerminate :: IO ()
foreign import ccall safe "wlc_run"       wlcRun       :: IO ()

foreign import ccall safe "wlc_get_backend_type" getBackendType :: IO BackendType

$(marshalImport' "wlc_handle_set_user_data" "setHandleUserdata'" [t|forall h a. WlcHandle h => h -> StablePtr a -> IO ()|] [t|Ptr () -> Ptr () -> IO ()|])
setHandleUserdata :: WlcHandle h => h -> a -> IO (IO ())
setHandleUserdata h d = do
  d' <- newStablePtr d
  setHandleUserdata' h d'
  return $ freeStablePtr d' -- return the finalizer

$(marshalImport' "wlc_handle_get_user_data" "getHandleUserdata'" [t|forall h a. WlcHandle h => h -> IO (StablePtr a)|] [t|Ptr () -> IO (Ptr ())|])
getHandleUserdata :: WlcHandle h => h -> IO a
getHandleUserdata = deRefStablePtr <=< getHandleUserdata'
