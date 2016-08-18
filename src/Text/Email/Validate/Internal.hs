{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Profunctor (lmap)
import Data.Profunctor.Product.Default (Default(def))
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Database.PostgreSQL.Simple.FromField
    ( Conversion, FieldParser, FromField(..), ResultError(..), returnError )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Opaleye
    ( Column, Constant(..), PGText, QueryRunnerColumn
    , QueryRunnerColumnDefault(..) , fieldQueryRunnerColumn )

import qualified "email-validate" Text.Email.Validate as EmailValidate

-- | Type to represent an email address.  Newtype wrapper around
-- 'EmailValidate.EmailAddress' with additional typeclass instances.
newtype EmailAddress = EmailAddress
    { unEmailAddress :: EmailValidate.EmailAddress }
    deriving (Data, Eq, Generic, Ord, Typeable)

instance Show EmailAddress where
    show :: EmailAddress -> String
    show = show . unEmailAddress

instance QueryRunnerColumnDefault PGText EmailAddress where
    queryRunnerColumnDefault :: QueryRunnerColumn PGText EmailAddress
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField EmailAddress where
    fromField :: FieldParser EmailAddress
    -- fromField :: Field -> Maybe ByteString -> Conversion EmailAddress
    fromField field Nothing = returnError UnexpectedNull field ""
    fromField field (Just email) = maybe err pure $ emailAddress email
      where
        err :: Conversion EmailAddress
        err = returnError ConversionFailed field $
            "Could not convert " <> show email <> " to email address"

instance Default Constant EmailAddress (Column PGText) where
    def :: Constant EmailAddress (Column PGText)
    def = lmap (decodeUtf8With lenientDecode . toByteString) def

-- | Parse 'EmailAddress' from JSON.
instance FromJSON EmailAddress where
    parseJSON :: Value -> Parser EmailAddress
    parseJSON = withText "EmailAddress" $ \t ->
                    case validate $ encodeUtf8 t of
                        Left err -> fail $ "Failed to parse email address: " <> err
                        Right email -> return email
    {-# INLINE parseJSON #-}

-- | Turn 'EmailAddress' into JSON.
instance ToJSON EmailAddress where
    toJSON :: EmailAddress -> Value
    toJSON = String . decodeUtf8With lenientDecode . toByteString

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
