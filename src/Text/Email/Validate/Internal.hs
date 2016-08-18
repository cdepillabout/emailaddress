{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
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

-- | Type to represent an email address.  Newtype wrapper around
-- 'EmailValidate.EmailAddress' with additional typeclass instances.
newtype EmailAddress = EmailAddress
    { unEmailAddress :: EmailValidate.EmailAddress }
    deriving (Data, Eq, Generic, Ord, Typeable)

instance Show EmailAddress where
    show :: EmailAddress -> String
    show = show . unEmailAddress

-- | Wrapper around 'EmailValidate.validate'.
--
-- >>> validate "foo@gmail.com"
-- Right "foo@gmail.com"
-- >>> import Data.Either (isLeft)
-- >>> isLeft $ validate "not an email address"
-- True
validate :: ByteString -> Either String EmailAddress
validate = fmap EmailAddress . EmailValidate.validate

-- | Wrapper around 'EmailValidate.emailAddress'.
--
-- Similar to 'validate', but returns 'Nothing' if the email address fails to
-- parse.
--
-- >>> emailAddress "foo@gmail.com"
-- Just "foo@gmail.com"
-- >>> emailAddress "not an email address"
-- Nothing
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress = fmap EmailAddress . EmailValidate.emailAddress

-- | Wrapper around 'EmailValidate.unsafeEmailAddress'.
--
-- Unsafely create an 'EmailAddress' from a local part and a domain part.  The
-- first argument is the local part, and the second argument is the domain
-- part.
--
-- For example, in the email address @foo\@gmail.com@, the local part is @foo@
-- and the domain part is @gmail.com@.
--
-- >>> unsafeEmailAddress "foo" "gmail.com"
-- "foo@gmail.com"
unsafeEmailAddress
    :: ByteString    -- ^ Local part
    -> ByteString    -- ^ Domain part
    -> EmailAddress
unsafeEmailAddress = (EmailAddress .) . EmailValidate.unsafeEmailAddress

-- | Wrapper around 'EmailValidate.localPart'.
--
-- Extracts the local part from an email address.
--
-- For example, in the email address @foo\@gmail.com@, the local part is @foo@.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> localPart email
-- "foo"
localPart :: EmailAddress -> ByteString
localPart = EmailValidate.localPart . unEmailAddress

-- | Wrapper around 'EmailValidate.domainPart'.
--
-- Extracts the domain part from an email address.
--
-- For example, in the email address @foo\@gmail.com@, the domain part is
-- @gmail.com@.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> domainPart email
-- "gmail.com"
domainPart :: EmailAddress -> ByteString
domainPart = EmailValidate.domainPart . unEmailAddress

-- | Wrapper around 'EmailValidate.toByteString'.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> toByteString email
-- "foo@gmail.com"
toByteString :: EmailAddress -> ByteString
toByteString = EmailValidate.toByteString . unEmailAddress
