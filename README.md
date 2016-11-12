
emailaddress
============

[![Build Status](https://secure.travis-ci.org/cdepillabout/emailaddress.svg)](http://travis-ci.org/cdepillabout/emailaddress)
[![Hackage](https://img.shields.io/hackage/v/emailaddress.svg)](https://hackage.haskell.org/package/emailaddress)
[![Stackage LTS](http://stackage.org/package/emailaddress/badge/lts)](http://stackage.org/lts/package/emailaddress)
[![Stackage Nightly](http://stackage.org/package/emailaddress/badge/nightly)](http://stackage.org/nightly/package/emailaddress)

This Haskell module wraps around the
[`email-validate`](https://hackage.haskell.org/package/email-validate) package,
providing a newtype wrapper around the `EmailAddress` type. Our `EmailAddress`
type has additional typeclass instances, including aeson's
[ToJSON](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:ToJSON)
and
[FromJSON](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:FromJSON).
This allows us to use the `EmailAddress` type without fear of orphan instances.

This package exposes the module `Text.EmailAddress`. It can be used as a
drop-in replacement for
[`email-validate`](https://hackage.haskell.org/package/email-validate).

If you would like to add new instances for `EmailAddress`, please send a pull
request.  Any instace for a typeclass from a package in stackage will be
accepted.
