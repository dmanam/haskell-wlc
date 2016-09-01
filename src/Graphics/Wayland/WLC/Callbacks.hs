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
  , module X
  ) where

import Graphics.Wayland.WLC.Callbacks.TH

import Graphics.Wayland.WLC.Geometry as X
import Graphics.Wayland.WLC.Types.Internal
import qualified Graphics.Wayland.WLC.Types as X

import Foreign
import Foreign.C

import Control.Applicative ((<*>))
import Control.Monad (join)

setCallback :: (CallbackType cbt, WrapFun (Callback cbt)) => cbt -> Callback cbt -> IO (IO ())
setCallback cbt f = do
  f' <- wrapFun f
  setCallback' cbt f'
  return (freeHaskellFunPtr f') -- return the finalizer in case they want to change the callback later

type OutputNilCb       = Output -> IO ()
type OutputBoolCb      = Output -> IO Bool
type OutputFocusCb     = Output -> Bool -> IO ()
type OutputResCb       = Output -> Size -> Size -> IO ()
type ViewNilCb         = View -> IO ()
type ViewBoolCb        = View -> IO Bool
type ViewFocusCb       = View -> Bool -> IO ()
type ViewMoveCb        = View -> Output -> Output -> IO ()
type ViewReqGeomCb     = View -> Geometry -> IO ()
type ViewReqStateCb    = View -> BitSet ViewState -> Bool -> IO ()
type ViewReqMoveCb     = View -> Point -> IO ()
type ViewReqResizeCb   = View -> BitSet Edge -> Point -> IO ()
type ViewPropsUpdateCb = View -> BitSet ViewProperty -> IO ()
type KeyboardKeyCb     = View -> Time -> BitSet Mod -> Key -> KeyState -> IO Bool
type PointerButtonCb   = View -> Time -> BitSet Mod -> Button -> ButtonState -> Point -> IO Bool
type PointerScrollCb   = View -> Time -> BitSet Mod -> BitSet ScrollAxis -> ScrollAmount -> IO Bool
type PointerMotionCb   = View -> Time -> Point -> IO Bool
type TouchCb           = View -> Time -> BitSet Mod -> TouchType -> Slot -> Point -> IO Bool
type NilCb             = IO ()
type InputBoolCb       = Input -> IO Bool
type InputNilCb        = Input -> IO ()
type LogHandlerCb      = LogType -> String -> IO ()

$(mkCallback' "OutputCreated"          ''OutputBoolCb)
$(mkCallback' "OutputDestroyed"        ''OutputNilCb)
$(mkCallback' "OutputFocus"            ''OutputFocusCb)
$(mkCallback' "OutputResolution"       ''OutputResCb)
$(mkCallback' "OutputRenderPre"        ''OutputNilCb)
$(mkCallback' "OutputRenderPost"       ''OutputNilCb)
$(mkCallback' "OutputContextCreated"   ''OutputNilCb)
$(mkCallback' "OutputContextDestroyed" ''OutputNilCb)
$(mkCallback' "ViewCreated"            ''ViewBoolCb)
$(mkCallback' "ViewDestroyed"          ''ViewNilCb)
$(mkCallback' "ViewFocus"              ''ViewFocusCb)
$(mkCallback' "ViewMove"               ''ViewMoveCb)
$(mkCallback' "ViewRequestGeometry"    ''ViewReqGeomCb)
$(mkCallback' "ViewRequestState"       ''ViewReqStateCb)
$(mkCallback' "ViewRequestMove"        ''ViewReqMoveCb)
$(mkCallback' "ViewRequestResize"      ''ViewReqResizeCb)
$(mkCallback' "ViewRenderPre"          ''ViewNilCb)
$(mkCallback' "ViewRenderPost"         ''ViewNilCb)
$(mkCallback' "ViewPropertiesUpdated"  ''ViewPropsUpdateCb)
$(mkCallback' "KeyboardKey"            ''KeyboardKeyCb)
$(mkCallback' "PointerButton"          ''PointerButtonCb)
$(mkCallback' "PointerScroll"          ''PointerScrollCb)
$(mkCallback' "PointerMotion"          ''PointerMotionCb)
$(mkCallback' "Touch"                  ''TouchCb)
$(mkCallback' "CompositorReady"        ''NilCb)
$(mkCallback' "CompositorTerminate"    ''NilCb)
$(mkCallback' "InputCreated"           ''InputBoolCb)
$(mkCallback' "InputDestroyed"         ''InputNilCb)

$(mkCallback "LogHandler" "wlc_log_set_handler" ''LogHandlerCb)
