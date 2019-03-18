{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      : $Header$
Description : ioctl wrapping
Copyright   : (c) Maciej Piechotka
License     : BSD3

Stability   : none
Portability : portable

The module provides a type-safe mechanism for ioctl calls.
-}
module System.IOControl
  (
    IOControl(..),
  )
where

import Foreign
import Foreign.C

-- | Combines the request with data.
class Storable d => IOControl req d | req -> d where
    -- | Converts request to integer
    ioctlReq :: req -- ^ The request. Should be lazy in argument.
             -> CInt -- ^ The request code.

