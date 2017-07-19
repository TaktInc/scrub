Scrub
=====

This package provides a type class called `Scrub` that is similar to `Show`
except it outputs `Text` and is specifically intended for use in places where
you need to scrub sensitive data from the output.  This is useful in situations
such as logging when you need to suppress PII/SPI, debugging when you might be
printing config data that could include passwords, or any other situation
where you need a string representation of a data type that is sanitized of
sensitive data.

Built-in instances are supplied for common core data types and then you are
expected to make newtype wrappers with appropriate instances for anything
sensitive in your data structures.

