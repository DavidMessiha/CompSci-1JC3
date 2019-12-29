{-# LANGUAGE FlexibleContexts,FlexibleInstances,IncoherentInstances #-}
{- Assignment 1 Extra Credit
 - Name: David Maher Messiha
 - Date: October 21, 2019
 -}
module Assign_2_ExtraCredit where

import Data.Complex

macid = "mahermed"


data GaussianInt a = a :@ a
  deriving (Show,Eq)

class GaussianIntegral g where
  gaussZero :: Integral a => g a
  gaussReal :: Integral a => g a -> a
  gaussImag :: Integral a => g a -> a
  gaussConj :: Integral a => g a -> g a
  gaussAdd :: Integral a => g a -> g a -> g a
  gaussMult :: Integral a => g a -> g a -> g a

{- TODO
 -   implement instances of GaussianIntegral
 -   implmenet instances of Eq, Ord
 -}

gaussNorm :: (Integral a, GaussianIntegral g) => g a -> a
gaussNorm g = error "TODO: implement gaussNorm"

maxGaussNorm :: (Integral a, GaussianIntegral g) => [g a] -> g a
maxGaussNorm gs = error "TODO: implement maxGaussNorm"
