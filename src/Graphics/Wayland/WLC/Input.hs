{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Wayland.WLC.Input
  (
  ) where

import Graphics.Wayland.WLC.Geometry
import Graphics.Wayland.WLC.Types.Internal
import Graphics.Wayland.WLC.Internal.TH

import Text.XkbCommon.InternalTypes

import Foreign
import Foreign.C

foreign import ccall safe "wlc_keyboard_get_xkb_state" c_getKeyboardState :: IO (Ptr CKeyboardState)
getKeyboardState :: IO KeyboardState
getKeyboardState = fmap toKeyboardState $ newForeignPtr_ =<< c_getKeyboardState

foreign import ccall safe "wlc_keyboard_get_xkb_keymap" c_getKeymap :: IO (Ptr CKeymap)
getKeymap :: IO Keymap
getKeymap = fmap toKeymap $ newForeignPtr_ =<< c_getKeymap
