{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Wayland.WLC.Callbacks
  ( CallbackType, Callback, setCallback
  , OutputCreated
  , OutputDestroyed
  , OutputFocus
  , OutputResolution
  , OutputRenderPre
  , OutputRenderPost
  , OutputContextCreated
  , OutputContextDestroyed
  , ViewCreated
  , ViewDestroyed
  , ViewFocus
  , ViewMove
  , ViewRequestGeometry
  , ViewRequestState
  , ViewRequestMove
  , ViewRequestResize
  , ViewRenderPre
  , ViewRenderPost
  , ViewPropertiesUpdated
  , KeyboardKey
  , PointerButton
  , PointerScroll
  , PointerMotion
  , Touch
  , CompositorReady
  , CompositorTerminate
  , InputCreated
  , InputDestroyed
  , LogHandler
  , module X
  ) where

import Graphics.Wayland.WLC.Callbacks.TH

import Graphics.Wayland.WLC.Geometry as X
import Graphics.Wayland.WLC.Types.Internal
import qualified Graphics.Wayland.WLC.Types as X

import Foreign
import Foreign.C

import System.Posix.Types (EpochTime)

import Control.Applicative ((<*>))
import Control.Monad (join)

setCallback :: (CallbackType cbt, WrapFun (Callback cbt)) => cbt -> Callback cbt -> IO (IO ())
setCallback cbt f = do
  f' <- wrapFun f
  setCallback' cbt f'
  return (freeHaskellFunPtr f') -- return the finalizer in case they want to change the callback later

$(mkCallback' "OutputCreated"          [t|Output -> IO Bool|])
$(mkCallback' "OutputDestroyed"        [t|Output -> IO ()|])
$(mkCallback' "OutputFocus"            [t|Output -> Bool -> IO ()|])
$(mkCallback' "OutputResolution"       [t|Output -> Size -> Size -> IO ()|])
$(mkCallback' "OutputRenderPre"        [t|Output -> IO ()|])
$(mkCallback' "OutputRenderPost"       [t|Output -> IO ()|])
$(mkCallback' "OutputContextCreated"   [t|Output -> IO ()|])
$(mkCallback' "OutputContextDestroyed" [t|Output -> IO ()|])
$(mkCallback' "ViewCreated"            [t|View -> IO Bool|])
$(mkCallback' "ViewDestroyed"          [t|View -> IO ()|])
$(mkCallback' "ViewFocus"              [t|View -> Bool -> IO ()|])
$(mkCallback' "ViewMove"               [t|View -> Output -> Output -> IO ()|])
$(mkCallback' "ViewRequestGeometry"    [t|View -> Geometry -> IO ()|])
$(mkCallback' "ViewRequestState"       [t|View -> BitSet ViewState -> Bool -> IO ()|])
$(mkCallback' "ViewRequestMove"        [t|View -> Point -> IO ()|])
$(mkCallback' "ViewRequestResize"      [t|View -> BitSet Edge -> Point -> IO ()|])
$(mkCallback' "ViewRenderPre"          [t|View -> IO ()|])
$(mkCallback' "ViewRenderPost"         [t|View -> IO ()|])
$(mkCallback' "ViewPropertiesUpdated"  [t|View -> BitSet ViewProperty -> IO ()|])
$(mkCallback' "KeyboardKey"            [t|View -> EpochTime -> BitSet Mod -> Key -> KeyState -> IO Bool|])
$(mkCallback' "PointerButton"          [t|View -> EpochTime -> BitSet Mod -> Button -> ButtonState -> Point -> IO Bool|])
$(mkCallback' "PointerScroll"          [t|View -> EpochTime -> BitSet Mod -> BitSet ScrollAxis -> ScrollAmount -> IO Bool|])
$(mkCallback' "PointerMotion"          [t|View -> EpochTime -> Point -> IO Bool|])
$(mkCallback' "Touch"                  [t|View -> EpochTime -> BitSet Mod -> TouchType -> Slot -> Point -> IO Bool|])
$(mkCallback' "CompositorReady"        [t|IO ()|])
$(mkCallback' "CompositorTerminate"    [t|IO ()|])
$(mkCallback' "InputCreated"           [t|Input -> IO Bool|])
$(mkCallback' "InputDestroyed"         [t|Input -> IO ()|])

$(mkCallback "LogHandler" "wlc_log_set_handler" [t|LogType -> String -> IO ()|])
