{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings  #-}

module Scrub
  ( Scrub(..)
  ) where

------------------------------------------------------------------------------
import           Data.Int
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Converts a type to Text while scrubbing it of sensitive data.
class Scrub a where
  scrub :: a -> Text

instance Scrub () where
  scrub a = "()"

instance Scrub String where
  scrub a = T.pack a

instance Scrub Text where
  scrub a = a

instance Scrub a => Scrub (Maybe a) where
  scrub Nothing = "Nothing"
  scrub (Just a) = "Just " <> scrub a

instance Scrub a => Scrub [a] where
  scrub [] = "[]"
  scrub as = "[" <> T.intercalate "," (map scrub as) <> "]"

------------------------------------------------------------------------------
-- Instances that piggy-back off Show
------------------------------------------------------------------------------

instance Scrub Bool where
  scrub a = T.pack $ show a

instance Scrub Char where
  scrub a = T.pack $ show a

instance Scrub Integer where
  scrub a = T.pack $ show a

instance Scrub Int where
  scrub a = T.pack $ show a

instance Scrub Int8 where
  scrub a = T.pack $ show a

instance Scrub Int16 where
  scrub a = T.pack $ show a

instance Scrub Int32 where
  scrub a = T.pack $ show a

instance Scrub Int64 where
  scrub a = T.pack $ show a

instance Scrub Word where
  scrub a = T.pack $ show a

instance Scrub Word8 where
  scrub a = T.pack $ show a

instance Scrub Word16 where
  scrub a = T.pack $ show a

instance Scrub Word32 where
  scrub a = T.pack $ show a

instance Scrub Word64 where
  scrub a = T.pack $ show a

instance Scrub Float where
  scrub a = T.pack $ show a

instance Scrub Double where
  scrub a = T.pack $ show a

instance Scrub Ordering where
  scrub a = T.pack $ show a


------------------------------------------------------------------------------
-- Tuples
------------------------------------------------------------------------------

instance (Scrub a, Scrub b) => Scrub (a,b) where
  scrub (a,b) = scrubTuple [scrub a, scrub b]

instance (Scrub a, Scrub b, Scrub c) => Scrub (a,b,c) where
  scrub (a,b,c) = scrubTuple [scrub a, scrub b, scrub c]

instance (Scrub a, Scrub b, Scrub c, Scrub d) => Scrub (a,b,c,d) where
  scrub (a,b,c,d) = scrubTuple [scrub a, scrub b, scrub c, scrub d]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e) => Scrub (a,b,c,d,e) where
  scrub (a,b,c,d,e) = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f) => Scrub (a,b,c,d,e,f) where
  scrub (a,b,c,d,e,f) = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g)
        => Scrub (a,b,c,d,e,f,g) where
  scrub (a,b,c,d,e,f,g)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h)
         => Scrub (a,b,c,d,e,f,g,h) where
  scrub (a,b,c,d,e,f,g,h)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i)
         => Scrub (a,b,c,d,e,f,g,h,i) where
  scrub (a,b,c,d,e,f,g,h,i)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i, Scrub j)
         => Scrub (a,b,c,d,e,f,g,h,i,j) where
  scrub (a,b,c,d,e,f,g,h,i,j)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i, scrub j]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i, Scrub j, Scrub k)
         => Scrub (a,b,c,d,e,f,g,h,i,j,k) where
  scrub (a,b,c,d,e,f,g,h,i,j,k)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i, scrub j, scrub k]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i, Scrub j, Scrub k,
          Scrub l)
         => Scrub (a,b,c,d,e,f,g,h,i,j,k,l) where
  scrub (a,b,c,d,e,f,g,h,i,j,k,l)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i, scrub j, scrub k, scrub l]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i, Scrub j, Scrub k,
          Scrub l, Scrub m)
         => Scrub (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  scrub (a,b,c,d,e,f,g,h,i,j,k,l,m)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i, scrub j, scrub k, scrub l, scrub m]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i, Scrub j, Scrub k,
          Scrub l, Scrub m, Scrub n)
         => Scrub (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  scrub (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i, scrub j, scrub k, scrub l, scrub m, scrub n]

instance (Scrub a, Scrub b, Scrub c, Scrub d, Scrub e, Scrub f, Scrub g, Scrub h, Scrub i, Scrub j, Scrub k,
          Scrub l, Scrub m, Scrub n, Scrub o)
         => Scrub (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  scrub (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
        = scrubTuple [scrub a, scrub b, scrub c, scrub d, scrub e, scrub f, scrub g, scrub h,
                      scrub i, scrub j, scrub k, scrub l, scrub m, scrub n, scrub o]

scrubTuple :: [Text] -> Text
scrubTuple ts = "(" <> T.intercalate "," ts <> ")"
