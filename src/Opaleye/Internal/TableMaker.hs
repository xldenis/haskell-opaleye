{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Opaleye.Internal.TableMaker where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.PackMap as PM

import           Data.Profunctor (Profunctor, dimap)
import           Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ


newtype ColumnMaker columns columns' =
  ColumnMaker (PM.PackMap HPQ.PrimExpr HPQ.PrimExpr columns columns')

runColumnMaker :: Applicative f
                  => ColumnMaker tablecolumns columns
                  -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                  -> tablecolumns -> f columns
runColumnMaker (ColumnMaker f) = PM.traversePM f

column :: ColumnMaker (C.Column a) (C.Column a)
column = ColumnMaker
         (PM.PackMap (\f (IC.Column s)
                      -> fmap IC.Column (f s)))

instance Default ColumnMaker (C.Column a) (C.Column a) where
  def = column

-- {

instance Functor (ColumnMaker a) where
  fmap f (ColumnMaker g) = ColumnMaker (fmap f g)

instance Applicative (ColumnMaker a) where
  pure = ColumnMaker . pure
  ColumnMaker f <*> ColumnMaker x = ColumnMaker (f <*> x)

instance Profunctor ColumnMaker where
  dimap f g (ColumnMaker q) = ColumnMaker (dimap f g q)

instance ProductProfunctor ColumnMaker where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

--}
