{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Graphics.Wayland.WLC.Core
  ( wlcInit, wlcTerminate, wlcRun
  , getBackendType
  , setHandleUserdata, getHandleUserdata
  , addFileEvent, addTimer, timerUpdate, removeFileEvent, removeTimer
  ) where

import Foreign
import Foreign.C

import Graphics.Wayland.WLC.Internal.TH
import Graphics.Wayland.WLC.Types.Internal

import Control.Monad (join, (<=<))
import Control.Applicative ((<*>))

import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types
import System.Posix.IO (handleToFd, fdToHandle)

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

runPtr p = do
  a <- deRefStablePtr $ castPtrToStablePtr p
  a :: IO ()
  return 0

foreign import ccall safe "wlc_event_loop_add_fd" c_addFileEvent :: Fd -> BitSet FileStatus -> FunPtr (Fd -> BitSet FileStatus -> Ptr () -> IO Int) -> Ptr () -> IO (Ptr ())
addFileEvent :: Handle -> BitSet FileStatus -> IO () -> IO FileEvent
addFileEvent h m e = do
  fd <- handleToFd h
  e' <- newStablePtr e
  p <- c_addFileEvent fd m fileEventHelper $ castStablePtrToPtr e'
  return $ FileEvent p e'
foreign import ccall safe "wrapper" fileEventHelperWrapper :: (Fd -> BitSet FileStatus -> Ptr () -> IO Int) -> IO (FunPtr (Fd -> BitSet FileStatus -> Ptr () -> IO Int))
{-# NOINLINE fileEventHelper #-}
fileEventHelper :: FunPtr (Fd -> BitSet FileStatus -> Ptr () -> IO Int)
fileEventHelper = unsafePerformIO . fileEventHelperWrapper . const . const $ runPtr

foreign import ccall safe "wlc_event_loop_add_timer" c_addTimer :: FunPtr (Ptr () -> IO Int) -> Ptr () -> IO (Ptr ())
addTimer :: IO () -> IO Timer
addTimer t = do
  t' <- newStablePtr t
  p <- c_addTimer timerHelper $ castStablePtrToPtr t'
  return $ Timer p t'
foreign import ccall safe "wrapper" timerHelperWrapper :: (Ptr () -> IO Int) -> IO (FunPtr (Ptr () -> IO Int))
{-# NOINLINE timerHelper #-}
timerHelper :: FunPtr (Ptr () -> IO Int)
timerHelper = unsafePerformIO $ timerHelperWrapper runPtr

foreign import ccall safe "wlc_event_source_timer_update" c_timerUpdate :: Ptr () -> MsTime -> IO Bool
timerUpdate :: Timer -> MsTime -> IO Bool
timerUpdate (Timer es _) = c_timerUpdate es

foreign import ccall safe "wlc_event_source_remove" c_removeEventSource :: Ptr () -> IO ()
removeEventSource :: Ptr () -> StablePtr (IO ()) -> IO ()
removeEventSource es ap = do
  c_removeEventSource es
  freeStablePtr ap

removeFileEvent (FileEvent es ap) = removeEventSource es ap
removeTimer     (Timer     es ap) = removeEventSource es ap
