module Graphics.Wayland.WLC.Types.Internal where

import Graphics.Wayland.WLC.Geometry

import Foreign
import Foreign.C

import Data.Function (on)

import Data.Bits
import Data.Default

#include <wlc/wlc.h>
#if __GLASGOW_HAKSELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x_; t (y_); }, y_)
#endif

#let mkEnumType name = "newtype %s = %s { un%s :: IntEnum } deriving (Show, Read, Eq, Ord)", #name, #name, #name
#let mkMaskBits name = "instance Bits %s where { (.|.) = (%s .) . on (.|.) un%s; (.&.) = (%s .) . on (.&.) un%s; xor = (%s .) . on xor un%s; complement = undefined; shift = undefined; rotate = undefined; bitSize = undefined; bitSizeMaybe = undefined; isSigned = undefined; testBit = undefined; bit = undefined; popCount = undefined }\ninstance Default %s where def = %s 0", #name, #name, #name, #name, #name, #name, #name, #name, #name

-- hack to make HLint work
#ifdef __GLASGOW_HASKELL__
#mkEnumType LogType
#{enum LogType, LogType
  , logInfo    = WLC_LOG_INFO
  , logWarn    = WLC_LOG_WARN
  , logError   = WLC_LOG_ERROR
  , logWayland = WLC_LOG_WAYLAND
  }

#mkEnumType BackendType
#{enum BackendType, BackendType
  , backendNone = WLC_BACKEND_NONE
  , backendDRM  = WLC_BACKEND_DRM
  , backendX11  = WLC_BACKEND_X11
  }

#mkEnumType EventTypeMask
#mkMaskBits EventTypeMask
#{enum EventTypeMask, EventTypeMask
  , eventReadable = WLC_EVENT_READABLE
  , eventWritable = WLC_EVENT_WRITABLE
  , eventHangup   = WLC_EVENT_HANGUP
  , eventError    = WLC_EVENT_ERROR
  }

#mkEnumType ViewStateMask
#mkMaskBits ViewStateMask
#{enum ViewStateMask, ViewStateMask
  , viewMaximized  = WLC_BIT_MAXIMIZED
  , viewFullscreen = WLC_BIT_FULLSCREEN
  , viewResizing   = WLC_BIT_RESIZING
  , viewMoving     = WLC_BIT_MOVING
  , viewActivated  = WLC_BIT_ACTIVATED
  }

#mkEnumType ViewTypeMask
#mkMaskBits ViewTypeMask
#{enum ViewTypeMask, ViewTypeMask
  , viewOverrideRedirect = WLC_BIT_OVERRIDE_REDIRECT
  , viewUnmanaged        = WLC_BIT_UNMANAGED
  , viewSplash           = WLC_BIT_SPLASH
  , viewModal            = WLC_BIT_MODAL
  , viewPopup            = WLC_BIT_POPUP
  }

#mkEnumType ViewPropertyMask
#mkMaskBits ViewPropertyMask
#{enum ViewPropertyMask, ViewPropertyMask
  , propertyTitle = WLC_BIT_PROPERTY_TITLE
  , propertyClass = WLC_BIT_PROPERTY_CLASS
  , propertyAppId = WLC_BIT_PROPERTY_APP_ID
  , propertyPid   = WLC_BIT_PROPERTY_PID
  }

#mkEnumType EdgeMask
#mkMaskBits EdgeMask
#{enum EdgeMask, EdgeMask
  , edgeNone        = WLC_RESIZE_EDGE_NONE
  , edgeTop         = WLC_RESIZE_EDGE_TOP
  , edgeBottom      = WLC_RESIZE_EDGE_BOTTOM
  , edgeLeft        = WLC_RESIZE_EDGE_LEFT
  , edgeTopLeft     = WLC_RESIZE_EDGE_TOP_LEFT
  , edgeBottomLeft  = WLC_RESIZE_EDGE_BOTTOM_LEFT
  , edgeRight       = WLC_RESIZE_EDGE_RIGHT
  , edgeTopRight    = WLC_RESIZE_EDGE_TOP_RIGHT
  , edgeBottomRight = WLC_RESIZE_EDGE_BOTTOM_RIGHT
  }

#mkEnumType ModMask
#mkMaskBits ModMask
#{enum ModMask, ModMask
  , modShift = WLC_BIT_MOD_SHIFT
  , modCaps  = WLC_BIT_MOD_CAPS
  , modCtrl  = WLC_BIT_MOD_CTRL
  , modAlt   = WLC_BIT_MOD_ALT
  , mod2     = WLC_BIT_MOD_MOD2
  , mod3     = WLC_BIT_MOD_MOD3
  , modLogo  = WLC_BIT_MOD_LOGO
  , mod5     = WLC_BIT_MOD_MOD5
  }

#mkEnumType LedMask
#mkMaskBits LedMask
#{enum LedMask, LedMask
  , ledNum    = WLC_BIT_LED_NUM
  , ledCaps   = WLC_BIT_LED_CAPS
  , ledScroll = WLC_BIT_LED_SCROLL
  }

#mkEnumType KeyState
#{enum KeyState, KeyState
  , keyReleased = WLC_KEY_STATE_RELEASED
  , keyPressed  = WLC_KEY_STATE_PRESSED
  }

#mkEnumType ButtonState
#{enum ButtonState, ButtonState
  , buttonReleased = WLC_BUTTON_STATE_RELEASED
  , buttonPressed  = WLC_BUTTON_STATE_PRESSED
  }

#mkEnumType ScrollAxisMask
#mkMaskBits ScrollAxisMask
#{enum ScrollAxisMask, ScrollAxisMask
  , axisVertical   = WLC_SCROLL_AXIS_VERTICAL
  , axisHorizontal = WLC_SCROLL_AXIS_HORIZONTAL
  }

#mkEnumType TouchType
#{enum TouchType, TouchType
  , touchDown   = WLC_TOUCH_DOWN
  , touchUp     = WLC_TOUCH_UP
  , touchMotion = WLC_TOUCH_MOTION
  , touchFrame  = WLC_TOUCH_FRAME
  , touchCancel = WLC_TOUCH_CANCEL
  }
#endif

type IntEnum = Word32

data Modifiers = Modifiers LedMask ModMask
data EventSource
data XkbState
data XkbKeymap
data InputDevice

data COutput
type Output = Ptr COutput
data CView
type View = Ptr CView

#ifdef __GLASGOW_HASKELL__
instance Storable Modifiers where
  alignment _ = #{alignment struct wlc_modifiers}
  sizeOf _ = #{size struct wlc_modifiers}
  peek ptr = do
    leds <- #{peek struct wlc_modifiers, leds} ptr
    mods <- #{peek struct wlc_modifiers, mods} ptr
    return $ Modifiers (LedMask leds) (ModMask mods)
  poke ptr (Modifiers leds mods) = do
    #{poke struct wlc_modifiers, leds} ptr $ unLedMask leds
    #{poke struct wlc_modifiers, mods} ptr $ unModMask mods
#endif
