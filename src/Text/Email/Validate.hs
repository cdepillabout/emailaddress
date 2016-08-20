{-|
Module      : Text.Email.Validate
Description : Wrapper around email-validate adding additional instances.
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD3

This module is a wrapper around
<https://hackage.haskell.org/package/email-validate-2.2.0/docs/Text-Email-Validate.html Text.Email.Validate>
from <https://hackage.haskell.org/package/email-validate email-validate>.

This module exports 'EmailAddress', a newtype wrapper around
<https://hackage.haskell.org/package/email-validate/docs/Text-Email-Validate.html#t:EmailAddress Text.Email.Validate.EmailAddress>.
Additional instances are defined for our
new 'EmailAddress', including 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON'. This is done so that no
orphan instances need to be used.

If you would like additional instances to be defined, please send a
<https://github.com/cdepillabout/emailaddress pull request>.  Additional
instances will be accepted for any typeclass from any package available on
stackage.
-}

module Text.Email.Validate
    ( -- * Data Type
      EmailAddress
      -- * Create EmailAddress
    , emailAddress
    , validate
    , emailAddressFromText
    , validateFromText
      -- * Check validity
    , isValid
      -- * Convert to Text
    , toText
      -- * Convert back to ByteString
    , toByteString
    , localPart
    , domainPart
      -- * Helper functions
    , canonicalizeEmail
      -- * Unsafe creation
    , unsafeEmailAddress
    ) where

import Text.Email.Validate.Internal
    ( EmailAddress, canonicalizeEmail, domainPart, emailAddress
    , emailAddressFromText, isValid, localPart, toByteString, toText, validate
    , validateFromText, unsafeEmailAddress )
