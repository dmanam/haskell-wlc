{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.WLC.View
  (
  ) where

import Graphics.Wayland.WLC.Geometry
import Graphics.Wayland.WLC.Types.Internal
import Graphics.Wayland.WLC.Internal.TH

import Foreign
import Foreign.C

import System.Posix.Types

foreign import ccall safe "wlc_view_focus" c_setFocusedView :: Ptr () -> IO ()
setFocusedView :: Maybe View -> IO ()
setFocusedView Nothing  = c_setFocusedView nullPtr
setFocusedView (Just v) = c_setFocusedView $ unView v

foreign import ccall safe "wlc_view_get_output" getOutput :: View -> IO Output

foreign import ccall safe "wlc_view_send_to_back" sendViewToBottom :: View -> IO ()

foreign import ccall safe "wlc_view_send_below" sendViewBelow :: View -> View -> IO ()

foreign import ccall safe "wlc_view_bring_above" sendViewAbove :: View -> View -> IO ()

foreign import ccall safe "wlc_view_bring_to_front" sendViewToTop :: View -> IO ()

foreign import ccall safe "wlc_view_set_mask" setViewMask :: View -> Word32 -> IO ()

$(marshalImport "wlc_view_get_geometry" "getViewGeometry" [t|View -> IO Geometry|])

foreign import ccall safe "wlc_view_get_visible_geometry" c_getViewVisibleGeometry :: View -> Ptr Geometry -> IO ()
getViewVisibleGeometry :: View -> IO Geometry
getViewVisibleGeometry v = alloca $ \gp -> do
  c_getViewVisibleGeometry v gp
  peek gp

foreign import ccall safe "wlc_view_set_geometry" c_setViewGeometry :: View -> BitSet Edge -> Ptr Geometry -> IO ()
setViewGeometry :: View -> BitSet Edge -> Geometry -> IO ()
setViewGeometry v es g = alloca $ \gp -> do
  poke gp g
  c_setViewGeometry v es gp

foreign import ccall safe "wlc_view_get_type" getViewType :: View -> IO (BitSet ViewType)

foreign import ccall safe "wlc_view_set_type" setViewType :: View -> BitSet ViewType -> Bool -> IO ()

foreign import ccall safe "wlc_view_get_state" getViewState :: View -> IO (BitSet ViewState)

foreign import ccall safe "wlc_view_set_state" setViewState :: View -> BitSet ViewState -> Bool -> IO ()

foreign import ccall safe "wlc_view_set_parent" setViewParent :: View -> View -> IO ()

$(marshalImport "wlc_view_get_title" "getViewTitle" [t|View -> IO String|])

$(marshalImport "wlc_view_get_class" "getViewClass" [t|View -> IO String|])

$(marshalImport "wlc_view_get_app_id" "getViewAppId" [t|View -> IO String|])

foreign import ccall safe "wlc_view_get_pid" getViewPid :: View -> IO ProcessID
