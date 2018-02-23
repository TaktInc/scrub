Scrub
=====

This package provides a type class called `Scrub`. This is useful in situations
such as logging when you need to suppress PII/SPI, debugging when you might be
printing config data that could include passwords, or any other situation
where you need a string representation of a data type that is sanitized of
sensitive data.

Built-in instances are supplied for common core data types and then you are
expected to make newtype wrappers with appropriate instances for anything
sensitive in your data structures.


## Example

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Scrub.Newtypes (Email(..), FullName(..), Sensitive(..))
import Scrub

data Person = Person
  { _pName  :: FullName
  , _pAge   :: Nat
  , _pEmail :: Email
  , _pRole  :: Role
  , _pBio   :: Sensitive Text }
  deriving (Show, Generic)

instance Scrub Person

frank :: Person
frank = Person "Frank Fuller" 43 "frank.fuller@ymail.com" "Real nice guy from Arizona" Admin
```

```haskell
> scrub frank
Scrubbed {unScrubbed = Person {_pName = FullName {unFullName = "<Scrubbed FullName>"}, _pAge = 43, _pEmail = Email {unEmail = "<Scrubbed Email>"}, pRole = Admin, _pBio = Sensitive {inSensitive = "<Scrubbed Sensitive Text>"}}}
```
