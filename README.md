
emailaddress
============

[![Hackage](https://img.shields.io/hackage/v/emailaddress.svg)](https://hackage.haskell.org/package/emailaddress) [![Build Status](https://secure.travis-ci.org/cdepillabout/emailaddress.svg)](http://travis-ci.org/cdepillabout/emailaddress)

This Haskell module wraps around the
[`email-validate`](https://hackage.haskell.org/package/email-validate) package,
providing a newtype wrapper around the `EmailAddress` type. Our `EmailAddress`
type has additional typeclass instances, including aeson's
[ToJSON](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON)
and
[FromJSON](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:FromJSON).

This module also exposes the module `Text.Email.Validate`, so it can be used as
a drop-in replacement for
[`email-validate`](https://hackage.haskell.org/package/email-validate).
