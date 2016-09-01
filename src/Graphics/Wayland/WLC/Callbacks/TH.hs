{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Wayland.WLC.Callbacks.TH
  ( WrapFun (..), mkWrapFun
  , CallbackType (..), mkCallback, mkCallback'
  ) where

import Graphics.Wayland.WLC.Types.Internal

import Language.Haskell.TH

import Foreign
import Foreign.C

import Control.Applicative ((<*>))
import Control.Monad (join, when)

import Data.Char (isUpper, toLower)

class WrapFun fn where
  wrapFun :: fn -> IO (FunPtr (Marshalled fn))

class CallbackType cbt where
  type Callback cbt
  setCallback' :: cbt -> FunPtr (Marshalled (Callback cbt)) -> IO ()

mkWrapFun :: Name -> Q [Dec]
mkWrapFun name = do
  TyConI (TySynD name [] t) <- reify name
  let wrapName = mkName $ "wrap" ++ nameBase name
      fnName = mkName "fn"
      appMarshal t = UInfixE (VarE wrapName) (VarE '($)) $ LamE ps $ UInfixE (VarE 'join) (VarE '($)) expr where
        (n, expr) = appMarshal' numns t $ AppE (VarE 'pure) (VarE fnName)
        ps = VarP <$> takeWhile (/= n) numns
      numns = mkName . ('x':) . show <$> [0..]
      appMarshal' (n:ns) (AppT (AppT ArrowT _) b) = appMarshal' ns b . \x -> UInfixE x (VarE '(<*>)) $ AppE (VarE 'unmarshal) (VarE n)
      appMarshal' (n:_) _ = \expr -> (n, expr)
  return
    [ ForeignD . ImportF CCall Safe "wrapper" wrapName $ AppT (AppT ArrowT $ AppT (ConT ''Marshalled) t) $ AppT (ConT ''IO) $ AppT (ConT ''FunPtr) $ AppT (ConT ''Marshalled) t
    , InstanceD [] (AppT (ConT ''WrapFun) t)
        [FunD 'wrapFun [Clause [VarP fnName] (NormalB $ appMarshal t) []]]
    ]

mkCallback :: String -> String -> Name -> Q [Dec]
mkCallback str cfn fn = do
  TyConI (TySynD fn [] fnt) <- reify fn
  let name = mkName str
      cName = mkName $ "on" ++ str
  alreadyWrapped <- isInstance ''WrapFun [fnt]
  wrapper <- if alreadyWrapped then return [] else mkWrapFun fn
  return $ wrapper ++
    [ ForeignD $ ImportF CCall Safe cfn cName
        $ AppT (AppT ArrowT $ AppT (ConT ''FunPtr) $ AppT (ConT ''Marshalled) fnt) $ AppT (ConT ''IO) $ TupleT 0
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
