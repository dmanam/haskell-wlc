{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.WLC.Output
  ( getOutputs, getFocusedOutput, setFocusedOutput
  , getOutputName
  , getOutputSleep, setOutputSleep
  , getOutputResolution, getOutputVirtualResolution, setOutputResolution, getOutputScale
  , getOutputMask, setOutputMask
  , getViews, setViews
  ) where

import Graphics.Wayland.WLC.Geometry
import Graphics.Wayland.WLC.Types.Internal
import Graphics.Wayland.WLC.Internal.TH

import Foreign
import Foreign.C

getArrayFn :: Storable a => (Ptr CSize -> IO (Ptr a)) -> IO [a]
getArrayFn f = alloca $ \s -> do
  ptr <- f s
  s' <- peek s
  peekArray (fromIntegral $ toInteger s') ptr

foreign import ccall safe "wlc_get_outputs" c_getOutputs :: Ptr CSize -> IO (Ptr (Ptr ()))
getOutputs :: IO [Output]
getOutputs = fmap Output <$> getArrayFn c_getOutputs

foreign import ccall safe "wlc_get_focused_output" getFocusedOutput :: IO Output

foreign import ccall safe "wlc_output_focus" setFocusedOutput :: Output -> IO ()

$(marshalImport "wlc_output_get_name" "getOutputName" [t|Output -> IO String|])

foreign import ccall safe "wlc_output_get_sleep" getOutputSleep :: Output -> IO Bool

foreign import ccall safe "wlc_output_set_sleep" setOutputSleep :: Output -> Bool -> IO ()

$(marshalImport "wlc_output_get_resolution" "getOutputResolution" [t|Output -> IO Size|])

$(marshalImport "wlc_output_get_virtual_resolution" "getOutputVirtualResolution" [t|Output -> IO Size|])

foreign import ccall safe "wlc_output_set_resolution" c_setOutputResolution :: Output -> Ptr Size -> Scale -> IO ()
setOutputResolution o sz sc = alloca $ \p -> do
  poke p sz
  c_setOutputResolution o p sc

foreign import ccall safe "wlc_output_get_scale" getOutputScale :: Output -> IO Scale

foreign import ccall safe "wlc_output_get_mask" getOutputMask :: Output -> IO Word32

foreign import ccall safe "wlc_output_set_mask" setOutputMask :: Output -> Word32 -> IO ()

foreign import ccall safe "wlc_output_get_views" c_getViews :: Output -> Ptr CSize -> IO (Ptr (Ptr ()))
getViews :: Output -> IO [View]
getViews = fmap (fmap View) . getArrayFn . c_getViews

foreign import ccall safe "wlc_output_set_views" c_setViews :: Output -> Ptr (Ptr ()) -> CSize -> IO ()
setViews :: Output -> [View] -> IO ()
setViews o vs = withArrayLen (fmap unView vs) $ \vsl vsp ->
  c_setViews o vsp (fromIntegral $ toInteger vsl)
