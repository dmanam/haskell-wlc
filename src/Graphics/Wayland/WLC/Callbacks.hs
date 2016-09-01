{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Wayland.WLC.Callbacks
  (
  ) where

import Graphics.Wayland.WLC.Callbacks.TH

import Graphics.Wayland.WLC.Geometry
import Graphics.Wayland.WLC.Types.Internal

import Foreign
import Foreign.C

import Control.Applicative ((<*>))
import Control.Monad (join)

setCallback :: (CallbackType cbt, WrapFun (Callback cbt)) => cbt -> Callback cbt -> IO (IO ())
setCallback cbt f = do
  f' <- wrapFun f
  setCallback' cbt f'
  return (freeHaskellFunPtr f') -- return the finalizer

type OutputNilCb       = Output -> IO ()
type OutputBoolCb      = Output -> IO Bool
type OutputFocusCb     = Output -> Bool -> IO ()
type OutputResCb       = Output -> Size -> Size -> IO ()
type ViewNilCb         = View -> IO ()
type ViewBoolCb        = View -> IO Bool
type ViewFocusCb       = View -> Bool -> IO ()
type ViewMoveCb        = View -> Output -> Output -> IO ()
type ViewReqGeomCb     = View -> Geometry -> IO ()
type ViewReqStateCb    = View -> ViewStateMask -> Bool -> IO ()
type ViewReqMoveCb     = View -> Point -> IO ()
type ViewReqResizeCb   = View -> EdgeMask -> Point -> IO ()
type ViewPropsUpdateCb = View -> ViewPropertyMask -> IO ()
$(mkWrapFun ''OutputNilCb)
$(mkWrapFun ''OutputBoolCb)
$(mkWrapFun ''OutputFocusCb)
-- manual   ''OutputResCb
$(mkWrapFun ''ViewNilCb)
$(mkWrapFun ''ViewBoolCb)
$(mkWrapFun ''ViewFocusCb)
$(mkWrapFun ''ViewMoveCb)
-- manual   ''ViewReqGeomCb
-- manual   ''ViewReqStateCb
-- manual   ''ViewReqMoveCb
-- manual   ''ViewReqResizeCb
-- manual   ''ViewPropsUpdateCb

foreign import ccall "wrapper" wrapOutputResCb :: CFun OutputResCb -> IO (FunPtr (CFun OutputResCb))
instance WrapFun OutputResCb where
  type CFun OutputResCb = Output -> Ptr Size -> Ptr Size -> IO ()
  wrapFun fn = wrapOutputResCb $ \out oldS newS -> join $ fn out <$> peek oldS <*> peek newS

foreign import ccall "wrapper" wrapViewReqGeomCb :: CFun ViewReqGeomCb -> IO (FunPtr (CFun ViewReqGeomCb))
instance WrapFun ViewReqGeomCb where
  type CFun ViewReqGeomCb = View -> Ptr Geometry -> IO ()
  wrapFun fn = wrapViewReqGeomCb $ \view geom -> fn view =<< peek geom

foreign import ccall "wrapper" wrapViewReqStateCb :: CFun ViewReqStateCb -> IO (FunPtr (CFun ViewReqStateCb))
instance WrapFun ViewReqStateCb where
  type CFun ViewReqStateCb = View -> IntEnum -> Bool -> IO ()
  wrapFun fn = wrapViewReqStateCb $ \view -> fn view . ViewStateMask

foreign import ccall "wrapper" wrapViewReqMoveCb :: CFun ViewReqMoveCb -> IO (FunPtr (CFun ViewReqMoveCb))
instance WrapFun ViewReqMoveCb where
  type CFun ViewReqMoveCb = View -> Ptr Point -> IO ()
  wrapFun fn = wrapViewReqMoveCb $ \view p -> fn view =<< peek p

foreign import ccall "wrapper" wrapViewReqResizeCb :: CFun ViewReqResizeCb -> IO (FunPtr (CFun ViewReqResizeCb))
instance WrapFun ViewReqResizeCb where
  type CFun ViewReqResizeCb = View -> IntEnum -> Ptr Point -> IO ()
  wrapFun fn = wrapViewReqResizeCb $ \view edges p -> fn view (EdgeMask edges) =<< peek p

foreign import ccall "wrapper" wrapViewPropsUpdateCb :: CFun ViewPropsUpdateCb -> IO (FunPtr (CFun ViewPropsUpdateCb))
instance WrapFun ViewPropsUpdateCb where
  type CFun ViewPropsUpdateCb = View -> IntEnum -> IO ()
  wrapFun fn = wrapViewPropsUpdateCb $ \view -> fn view . ViewPropertyMask

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
