{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

module Text.Email.Validate.Internal
    ( EmailAddress(unEmailAddress)
    , EmailValidate.isValid
    , EmailValidate.canonicalizeEmail
    , emailAddress
    , validate
    , unsafeEmailAddress
    , localPart
    , domainPart
    , toByteString
    ) where

import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified "email-validate" Text.Email.Validate as EmailValidate

newtype EmailAddress = EmailAddress
    { unEmailAddress :: EmailValidate.EmailAddress }
    deriving (Data, Eq, Generic, Ord, Read, Show, Typeable)

validate :: ByteString -> Either String EmailAddress
validate = fmap EmailAddress . EmailValidate.validate

emailAddress :: ByteString -> Maybe EmailAddress
emailAddress = fmap EmailAddress . EmailValidate.emailAddress

-- | Unsafely create an 'EmailAddress' from a local part and a domain part.
-- The first argument is the local part, and the second argument is the domain
-- part.
--
-- For example, in the email address @foo\@gmail.com@, the local part is @foo@
-- and the domain part is @gmail.com@.
--
-- >>> toByteString $ unsafeEmailAddress "foo" "gmail.com"
-- "foo@gmail.com"
unsafeEmailAddress
    :: ByteString    -- ^ Local part
    -> ByteString    -- ^ Domain part
    -> EmailAddress
unsafeEmailAddress = (EmailAddress .) . EmailValidate.unsafeEmailAddress

localPart :: EmailAddress -> ByteString
localPart = EmailValidate.localPart . unEmailAddress

domainPart :: EmailAddress -> ByteString
domainPart = EmailValidate.domainPart . unEmailAddress

toByteString :: EmailAddress -> ByteString
toByteString = EmailValidate.toByteString . unEmailAddress
