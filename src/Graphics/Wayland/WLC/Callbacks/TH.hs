{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Wayland.WLC.Callbacks.TH
  ( WrapFun (..), mkWrapFun
  , CallbackType (..), mkCallback, mkCallback'
  ) where

import Language.Haskell.TH

import Foreign
import Foreign.C

import Data.Char (isUpper, toLower)

class WrapFun fn where
  type CFun fn
  wrapFun :: fn -> IO (FunPtr (CFun fn))

class CallbackType cbt where
  type Callback cbt
  setCallback' :: cbt -> FunPtr (CFun (Callback cbt)) -> IO ()

mkWrapFun :: Name -> Q [Dec]
mkWrapFun name = do
  TyConI (TySynD name [] t) <- reify name
  let wrapName = mkName $ "wrap" ++ nameBase name
  return
    [ ForeignD . ImportF CCall Safe "wrapper" wrapName $ AppT (AppT ArrowT t) (AppT (ConT ''IO) $ AppT (ConT ''FunPtr) t)
    , InstanceD [] (AppT (ConT ''WrapFun) t)
        [ TySynInstD ''CFun $ TySynEqn [t] t
        , FunD 'wrapFun [Clause [] (NormalB $ VarE wrapName) []]
        ]
    ]

mkCallback :: String -> String -> Name -> Q [Dec]
mkCallback str cfn fn = do
  let name = mkName str
      fnt = ConT fn
      cName = mkName $ "on" ++ str
  return
    [ ForeignD $ ImportF CCall Safe cfn cName
        $ AppT (AppT ArrowT $ AppT (ConT ''FunPtr) $ AppT (ConT ''CFun) fnt) $ AppT (ConT ''IO) $ TupleT 0
    , DataD [] name [] [NormalC name []] []
    , InstanceD [] (AppT (ConT ''CallbackType) (ConT name))
        [ TySynInstD ''Callback $ TySynEqn [ConT name] fnt
        , FunD 'setCallback' [Clause [WildP] (NormalB $ VarE cName) []]
        ]
    ]

mkCallback' :: String -> Name -> Q [Dec]
mkCallback' str = mkCallback str cstr where
  cstr = "wlc_set" ++ (replFn =<< str) ++ "_cb"
  replFn c
    | isUpper c = ['_', toLower c]
    | otherwise = [c]
