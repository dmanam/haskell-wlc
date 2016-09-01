{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}

module Graphics.Wayland.WLC.Types.Internal where

import Graphics.Wayland.WLC.Geometry

import Foreign
import Foreign.C

import Control.Monad ((<=<), join)

import Data.Bits

#include <wlc/wlc.h>
#if __GLASGOW_HAKSELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x_; t (y_); }, y_)
#endif

#let mkNewtype name = "newtype %s = %s { un%s :: IntEnum } deriving (Show, Read, Eq, Ord)\ninstance HasMarshalled %s where type Marshalled %s = IntEnum\ninstance Marshallable %s where marshal = return . un%s\ninstance Unmarshallable %s where unmarshal = return . %s", #name, #name, #name, #name, #name, #name, #name, #name, #name
#let mkNewtypeSmall name = "newtype %s = %s { un%s :: IntEnumSmall } deriving (Show, Read, Eq, Ord)\ninstance HasMarshalled %s where type Marshalled %s = IntEnumSmall\ninstance Marshallable %s where marshal = return . un%s\ninstance Unmarshallable %s where unmarshal = return . %s", #name, #name, #name, #name, #name, #name, #name, #name, #name
#let mkMask name = "instance Mask %s where { mask = %s; unmask = un%s }", #name, #name, #name

-- hack to make HLint work
#ifdef __GLASGOW_HASKELL__
#mkNewtype LogType
#{enum LogType, LogType
  , logInfo    = WLC_LOG_INFO
  , logWarn    = WLC_LOG_WARN
  , logError   = WLC_LOG_ERROR
  , logWayland = WLC_LOG_WAYLAND
  }

#mkNewtype BackendType
#{enum BackendType, BackendType
  , backendNone = WLC_BACKEND_NONE
  , backendDRM  = WLC_BACKEND_DRM
  , backendX11  = WLC_BACKEND_X11
  }

#mkNewtype EventType
#mkMask EventType
#{enum EventType, EventType
  , eventReadable = WLC_EVENT_READABLE
  , eventWritable = WLC_EVENT_WRITABLE
  , eventHangup   = WLC_EVENT_HANGUP
  , eventError    = WLC_EVENT_ERROR
  }

#mkNewtype ViewState
#mkMask ViewState
#{enum ViewState, ViewState
  , viewMaximized  = WLC_BIT_MAXIMIZED
  , viewFullscreen = WLC_BIT_FULLSCREEN
  , viewResizing   = WLC_BIT_RESIZING
  , viewMoving     = WLC_BIT_MOVING
  , viewActivated  = WLC_BIT_ACTIVATED
  }

#mkNewtype ViewType
#mkMask ViewType
#{enum ViewType, ViewType
  , viewOverrideRedirect = WLC_BIT_OVERRIDE_REDIRECT
  , viewUnmanaged        = WLC_BIT_UNMANAGED
  , viewSplash           = WLC_BIT_SPLASH
  , viewModal            = WLC_BIT_MODAL
  , viewPopup            = WLC_BIT_POPUP
  }

#mkNewtype ViewProperty
#mkMask ViewProperty
#{enum ViewProperty, ViewProperty
  , propertyTitle = WLC_BIT_PROPERTY_TITLE
  , propertyClass = WLC_BIT_PROPERTY_CLASS
  , propertyAppId = WLC_BIT_PROPERTY_APP_ID
  , propertyPid   = WLC_BIT_PROPERTY_PID
  }

#mkNewtype Edge
#mkMask Edge
#{enum Edge, Edge
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

#mkNewtype Mod
#mkMask Mod
#{enum Mod, Mod
  , modShift = WLC_BIT_MOD_SHIFT
  , modCaps  = WLC_BIT_MOD_CAPS
  , modCtrl  = WLC_BIT_MOD_CTRL
  , modAlt   = WLC_BIT_MOD_ALT
  , mod2     = WLC_BIT_MOD_MOD2
  , mod3     = WLC_BIT_MOD_MOD3
  , modLogo  = WLC_BIT_MOD_LOGO
  , mod5     = WLC_BIT_MOD_MOD5
  }

#mkNewtype Led
#mkMask Led
#{enum Led, Led
  , ledNum    = WLC_BIT_LED_NUM
  , ledCaps   = WLC_BIT_LED_CAPS
  , ledScroll = WLC_BIT_LED_SCROLL
  }

#mkNewtype KeyState
#{enum KeyState, KeyState
  , keyReleased = WLC_KEY_STATE_RELEASED
  , keyPressed  = WLC_KEY_STATE_PRESSED
  }

#mkNewtype ButtonState
#{enum ButtonState, ButtonState
  , buttonReleased = WLC_BUTTON_STATE_RELEASED
  , buttonPressed  = WLC_BUTTON_STATE_PRESSED
  }

#mkNewtypeSmall ScrollAxis
instance Mask ScrollAxis where
  mask = ScrollAxis . fromInteger . toInteger
  unmask = fromInteger . toInteger . unScrollAxis
#{enum ScrollAxis, ScrollAxis
  , axisVertical   = WLC_SCROLL_AXIS_VERTICAL
  , axisHorizontal = WLC_SCROLL_AXIS_HORIZONTAL
  }

#mkNewtype TouchType
#{enum TouchType, TouchType
  , touchDown   = WLC_TOUCH_DOWN
  , touchUp     = WLC_TOUCH_UP
  , touchMotion = WLC_TOUCH_MOTION
  , touchFrame  = WLC_TOUCH_FRAME
  , touchCancel = WLC_TOUCH_CANCEL
  }
#endif

type IntEnumSmall = Word8
type IntEnum = Word32
type Time    = Word32
type Key     = Word32
type Button  = Word32
type Slot    = Word32

newtype ScrollAmount = ScrollAmount { unScrollAmount :: (Double, Double) }

data Modifiers = Modifiers (BitSet Led) (BitSet Mod)

data CEventSource
type EventSource = Ptr CEventSource
data CXkbState
type XkbState = Ptr CXkbState
data CXkbKeymap
type XkbKeymap = Ptr CXkbKeymap
data CInput
type Input = Ptr CInput
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
    return $ Modifiers (BitSet leds) (BitSet mods)
  poke ptr (Modifiers leds mods) = do
    #{poke struct wlc_modifiers, leds} ptr $ unBitSet leds
    #{poke struct wlc_modifiers, mods} ptr $ unBitSet mods
#endif

class Mask a where
  mask :: IntEnum -> a
  unmask :: a -> IntEnum

newtype BitSet a = BitSet { unBitSet :: IntEnum }

fromMaskList :: Mask a => [a] -> BitSet a
fromMaskList = BitSet . foldr ((.|.) . unmask) 0

bitContains :: Mask a => BitSet a -> a -> Bool
bitContains s a = s' .&. a' == a' where
  s' = unBitSet s
  a' = unmask a

class HasMarshalled a where
  type Marshalled a :: *
  type Marshalled a = a

class HasMarshalled a => Marshallable a where
  marshal :: a -> IO (Marshalled a)
  default marshal :: a -> IO a
  marshal = return

class HasMarshalled a => Unmarshallable a where
  unmarshal :: Marshalled a -> IO a
  default unmarshal :: a -> IO a
  unmarshal = return

instance HasMarshalled (Ptr a)
instance Marshallable (Ptr a)
instance Unmarshallable (Ptr a)

instance HasMarshalled Word32
instance Marshallable Word32
instance Unmarshallable Word32

instance HasMarshalled Bool
instance Marshallable Bool
instance Unmarshallable Bool

instance HasMarshalled ()
instance Marshallable ()
instance Unmarshallable ()

instance HasMarshalled (BitSet a) where
  type Marshalled (BitSet a) = Marshalled a
instance Num (Marshalled a) => Marshallable (BitSet a) where
  marshal = return . fromInteger . toInteger . unBitSet
instance Integral (Marshalled a) => Unmarshallable (BitSet a) where
  unmarshal = return . BitSet . fromInteger . toInteger

instance HasMarshalled a => HasMarshalled (IO a) where
  type Marshalled (IO a) = IO (Marshalled a)
instance Marshallable a => Marshallable (IO a) where
  --marshal = (marshal =<<)
  marshal = fmap marshal
instance Unmarshallable a => Unmarshallable (IO a) where
  --unmarshal = return . unmarshal
  unmarshal = fmap unmarshal

instance HasMarshalled (a -> b) where
  type Marshalled (a -> b) = Marshalled a -> Marshalled b

instance HasMarshalled String where
  type Marshalled String = CString
instance Unmarshallable String where
  unmarshal = peekCString

instance HasMarshalled ScrollAmount where
  type Marshalled ScrollAmount = Ptr Double
instance Unmarshallable ScrollAmount where
  unmarshal ptr = do
    [x, y] <- peekArray 2 ptr
    return $ ScrollAmount (x, y)

#let structMarshal t = "instance HasMarshalled %s where type Marshalled %s = Ptr %s\ninstance Unmarshallable %s where unmarshal = peek", #t, #t, #t, #t
#structMarshal Modifiers
#structMarshal Point
#structMarshal Size
#structMarshal Geometry
