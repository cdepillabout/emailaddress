module Text.Email.Validate
    ( -- * Data Type
      EmailAddress
      -- * Create EmailAddress
    , emailAddress
    , validate
      -- * Check validity
    , isValid
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
    ( EmailAddress, canonicalizeEmail, domainPart, emailAddress, isValid
    , localPart, toByteString, validate, unsafeEmailAddress )
