{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Scrub.Wrappers where

------------------------------------------------------------------------------
import           Data.Data
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           GHC.Generics
------------------------------------------------------------------------------
import           Scrub
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Newtype wrapper for Text email addresses with an appropriate Scrub
-- instance.
newtype Email = Email { unEmail :: Text }
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance IsString Email where
  fromString = Email . T.pack

instance Scrub Email where
  scrub _ = "<email scrubbed>"


------------------------------------------------------------------------------
-- | Newtype wrapper for Text passwords with an appropriate Scrub instance.
newtype Password = Password { unPassword :: Text }
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

instance Scrub Password where
  scrub _ = "<password scrubbed>"

instance IsString Password where
  fromString = Password . T.pack
