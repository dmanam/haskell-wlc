{-# LANGUAGE CPP #-}

module Graphics.Wayland.WLC.Geometry
  (Point(..), Size(..), Geometry(..), Lattice(..), contains, module Data.Default) where

import Foreign
import Foreign.C

import Data.Default

#include <wlc/geometry.h>
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

data Point = Point Word32 Word32
  deriving (Show, Read, Eq)
data Size  = Size  Word32 Word32
  deriving (Show, Read, Eq)
data Geometry = Geometry Point Size
  deriving (Show, Read, Eq)

-- hack to make HLint work
#ifdef __GLASGOW_HASKELL__
instance Storable Point where
  alignment _ = #{alignment struct wlc_point}
  sizeOf _ = #{size struct wlc_point}
  peek ptr = do
    x <- #{peek struct wlc_point, x} ptr
    y <- #{peek struct wlc_point, y} ptr
    return $ Point x y
  poke ptr (Point x y) = do
    #{poke struct wlc_point, x} ptr x
    #{poke struct wlc_point, y} ptr y
instance Storable Size where
  alignment _ = #{alignment struct wlc_size}
  sizeOf _ = #{size struct wlc_size}
  peek ptr = do
    w <- #{peek struct wlc_size, h} ptr
    h <- #{peek struct wlc_size, w} ptr
    return $ Size w h
  poke ptr (Size w h) = do
    #{poke struct wlc_size, w} ptr w
    #{poke struct wlc_size, h} ptr h
instance Storable Geometry where
  alignment _ = #{alignment struct wlc_geometry}
  sizeOf _ = #{size struct wlc_geometry}
  peek ptr = do
    p <- peek $ #{ptr struct wlc_geometry, origin} ptr
    s <- peek $ #{ptr struct wlc_geometry, size} ptr
    return $ Geometry p s
  poke ptr (Geometry p s) = do
    poke (#{ptr struct wlc_geometry, origin} ptr) p
    poke (#{ptr struct wlc_geometry, size}   ptr) s
#endif

instance Default Point where def = Point 0 0
instance Default Size  where def = Size  0 0
instance Default Geometry where def = Geometry def def

infixr 5 \/
infixr 6 /\\

class Lattice a where
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a
instance Lattice Word32 where
  (\/) = max
  (/\) = min
instance Lattice Point where
  Point x1 y1 \/ Point x2 y2 = Point (x1 \/ y1) (x2 \/ y2)
  Point x1 y1 /\ Point x2 y2 = Point (x1 /\ y1) (x2 /\ y2)
instance Lattice Size where
  Size x1 y1 \/ Size x2 y2 = Size (x1 \/ y1) (x2 \/ y2)
  Size x1 y1 /\ Size x2 y2 = Size (x1 /\ y1) (x2 /\ y2)

contains :: Geometry -> Geometry -> Bool
contains (Geometry p1 s1) (Geometry p2 s2)
  = p1 /\ p2 == p1 && p1' \/ p2' == p1' where
    p1' = p1 `plus` s1
    p2' = p2 `plus` s2
    plus (Point x y) (Size w h) = Point (x + w) (y + h)
