{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Scrub.Class
  ( Scrub(..)
  , Scrubbed(..)
  , scrubS, scrubI
  ) where

------------------------------------------------------------------------------
import           Data.Data
import           Data.Monoid
import           Data.String
import           GHC.Generics
------------------------------------------------------------------------------


-- | Newtype wrapper that document that a peice of data has been scrubbed.
newtype Scrubbed a = Scrubbed { getScrubbed :: a }
  deriving (Eq,Ord,Read,Show,Monoid,Typeable,Data,Generic)

class Scrub a where
  {-|
  Convert some data to a version scrubbed of sensitive information.
  This class comes with support for `Generic``s, such that derivied
  instances automatically scrub sensitive constituent data.
  -}

  scrub :: a -> Scrubbed a
  default scrub :: (Generic a, GScrub (Rep a)) => a -> Scrubbed a
  scrub = Scrubbed . to . gscrub . from

instance {-# OVERLAPPABLE #-} (Scrub a, Functor f) => Scrub (f a) where
  scrub = Scrubbed . fmap clean

instance (Scrub l, Scrub r) => Scrub (Either l r) where
  scrub (Left l)  = Scrubbed . Left $ clean l
  scrub (Right r) = Scrubbed . Right $ clean r

instance (Scrub l, Scrub r) => Scrub (l, r) where
  scrub (l, r) = Scrubbed (clean l, clean r)

instance {-# OVERLAPPABLE #-} Scrub a where
  scrub = Scrubbed

-- | A useful default for newtypes that wrap a string like structure.
-- We do not expose an @instance (IsString s, Monoid s, Typeable s) => Scrub s@
-- to avoid scrubbing strings we might wish to preserve.
scrubS :: (IsString s, Monoid s, Typeable s) => s -> Scrubbed s
scrubS x = Scrubbed $ "<Scrubbed " <> fromString (show (typeOf x)) <> ">"

-- | A useful default for newtypes that wrap a number like structure, (or more specifically @const scrubI@).
-- We do not expose an @instance Num n => Scrub n@ to avoid scrubbing
-- numbers we might wish to preserve.
scrubI :: Num n => Scrubbed n
scrubI = Scrubbed (negate 1)

class GScrub (g :: * -> *) where
  gscrub :: g a -> g a

instance GScrub U1 where
  gscrub = id

instance Scrub c => GScrub (K1 i c) where
  gscrub = K1 . clean . unK1

instance GScrub f => GScrub (M1 i c f) where
  gscrub (M1 x) = M1 (gscrub x)

instance (GScrub f, GScrub g) => GScrub (f :+: g) where
  gscrub (L1 l) = L1 (gscrub l)
  gscrub (R1 r) = R1 (gscrub r)

instance (GScrub f, GScrub g) => GScrub (f :*: g) where
  gscrub (l :*: r) = gscrub l :*: gscrub r

clean :: Scrub a => a -> a
clean = getScrubbed . scrub
