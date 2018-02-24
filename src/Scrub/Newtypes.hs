{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Scrub.Newtypes
  ( -- * Newtypes for common PII
    FullName(..)
  , Address(..)
  , Email(..)
  , NationalIdentificationNumber(..)
  , Passport(..)
  , VehicleRegistration(..)
  , DriversLicenseNumber(..)
  , CreditCardNumber(..)
  , DigitalIdentity(..)
  , BirthDay(..)
  , TelephoneNumber(..)
  , UserName(..)
  , Password(..)
  -- * Generic wrapper for easy tagging of data
  , Sensitive(..)
  ) where

------------------------------------------------------------------------------
import           Data.Data
import           Data.Int
import           Data.String
import           Data.Text          (Text)
import           Data.Time.Calendar
import           GHC.Generics
------------------------------------------------------------------------------
import           Scrub.Class
------------------------------------------------------------------------------

newtype FullName = FullName { unFullName :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub FullName where scrub = scrubS

newtype Address = Address { unAddress :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub Address where scrub = scrubS

newtype Email = Email { unEmail :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub Email where scrub = scrubS

newtype NationalIdentificationNumber = NationalIdentificationNumber { unNationalIdentificationNumber :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub NationalIdentificationNumber where scrub = scrubS

newtype Passport = Passport { unPassport :: Int64 }
  deriving (Show,Eq,Ord,Typeable,Data,Generic,Num)
instance Scrub Passport where scrub _ = scrubI

newtype VehicleRegistration = VehicleRegistration { unVehicleRegistration :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub VehicleRegistration where scrub = scrubS

newtype DriversLicenseNumber = DriversLicenseNumber { unDriversLicenseNumber :: Int64 }
  deriving (Show,Eq,Ord,Typeable,Data,Generic,Num)
instance Scrub DriversLicenseNumber where scrub _ = scrubI

newtype CreditCardNumber = CreditCardNumber { unCreditCardNumber :: [Int] }
  deriving (Show,Eq,Ord,Typeable,Data,Generic)
instance Scrub CreditCardNumber where scrub = Scrubbed . CreditCardNumber . getScrubbed . scrub . unCreditCardNumber

newtype DigitalIdentity = DigitalIdentity { unDigitalIdentity :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub DigitalIdentity where scrub = scrubS

newtype BirthDay = BirthDay { unBirthDay :: Day }
  deriving (Show,Eq,Ord,Typeable,Data,Generic)
instance Scrub BirthDay where scrub _ = Scrubbed . BirthDay . ModifiedJulianDay $ negate 1

newtype TelephoneNumber = TelephoneNumber { unTelephoneNumber :: Int64 }
  deriving (Show,Eq,Ord,Typeable,Data,Generic,Num)
instance Scrub TelephoneNumber where scrub _ = scrubI

newtype UserName = UserName { unUserName :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub UserName where scrub = scrubS

newtype Password = Password { unPassword :: Text }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString)
instance Scrub Password where scrub = scrubS

newtype Sensitive a = Sensitive { inSensitive :: a }
  deriving (Show,Eq,Ord,Monoid,Typeable,Data,Generic,IsString,Num)
instance (IsString s, Monoid s, Typeable s) => Scrub (Sensitive s) where scrub = scrubS
instance Scrub (Sensitive Int) where scrub _ = scrubI
instance Scrub (Sensitive Float) where scrub _ = scrubI
instance Scrub (Sensitive Int64) where scrub _ = scrubI
instance Scrub (Sensitive Day) where scrub _ = Scrubbed . Sensitive . ModifiedJulianDay $ negate 1
