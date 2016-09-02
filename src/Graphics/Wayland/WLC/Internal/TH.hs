{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.WLC.Internal.TH
  ( beforeAfterFn, beforeAfterFnt
  , fmarshal, funmarshal
  , marshalImport, marshalImport'
  ) where

import Graphics.Wayland.WLC.Types.Internal

import Language.Haskell.TH

import Data.Char (toLower)

import Control.Applicative ((<*>))
import Control.Monad (join)

beforeAfterFn :: Name -> Name -> Name -> Q Exp
beforeAfterFn before after fn = do
  VarI _ fnt _ _ <- reify fn
  return $ beforeAfterFnt before after fn fnt

beforeAfterFnt :: Name -> Name -> Name -> Type -> Exp
beforeAfterFnt before after fn fnt = ParensE . appBefore $ stripCxt fnt where
  appBefore t = LamE ps $ UInfixE (UInfixE (VarE after) (VarE '(.)) (VarE 'join)) (VarE '($)) expr where
    (n, expr) = appBefore' numns t $ AppE (VarE 'pure) (VarE fn)
    ps = VarP <$> takeWhile (/= n) numns
  numns = mkName . ('x':) . show <$> [0..]
  appBefore' (n:ns) (AppT (AppT ArrowT _) b) = appBefore' ns b . \x -> UInfixE x (VarE '(<*>)) $ AppE (VarE before) (VarE n)
  appBefore' (n:_) _ = \expr -> (n, expr)
  stripCxt (ForallT _ _ t) = t
  stripCxt t = t

unmarshal' :: Unmarshallable a => IO (Marshalled a) -> IO a
unmarshal' = (unmarshal =<<)
marshal' :: Marshallable a => IO a -> IO (Marshalled a)
marshal' = (marshal =<<)

fmarshal   = beforeAfterFn 'unmarshal   'marshal'
funmarshal = beforeAfterFn   'marshal 'unmarshal'

marshalImport :: String -> String -> Q Type -> Q [Dec]
marshalImport cfnstr fnstr fnt = marshalImport' cfnstr fnstr fnt $ AppT (ConT ''Marshalled) <$> fnt

marshalImport' :: String -> String -> Q Type -> Q Type -> Q [Dec]
marshalImport' cfnstr fnstr fnt' cfnt' = do
  fnt  <- fnt'
  cfnt <- cfnt'
  let fn = mkName fnstr
      cfn = mkName $ "c_" ++ fnstr
      fndef = beforeAfterFnt 'marshal 'unmarshal' cfn fnt
  return
    [ ForeignD $ ImportF CCall Safe cfnstr cfn cfnt
    , SigD fn fnt
    , FunD fn [Clause [] (NormalB fndef) []]
    ]
