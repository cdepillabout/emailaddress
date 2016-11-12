{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.EmailAddress.Internal
    ( EmailAddress(EmailAddress, unEmailAddress)
    , EmailValidate.isValid
    , EmailValidate.canonicalizeEmail
    , emailAddress
    , validate
    , emailAddressFromText
    , validateFromText
    , emailAddressFromString
    , validateFromString
    , unsafeEmailAddress
    , localPart
    , domainPart
    , toByteString
    , toText
    ) where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Profunctor (lmap)
import Data.Profunctor.Product.Default (Default(def))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist (PersistField(..), PersistValue)
import Database.Persist.Sql (PersistFieldSql(..), SqlType)
import Database.PostgreSQL.Simple.FromField
    ( Conversion, FieldParser, FromField(..), ResultError(..), returnError )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Opaleye
    ( Column, Constant(..), PGText, QueryRunnerColumn
    , QueryRunnerColumnDefault(..) , fieldQueryRunnerColumn )
import Text.Read (Read(readPrec), ReadPrec)
import Web.HttpApiData
    ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece) )
import Web.PathPieces (PathPiece(fromPathPiece, toPathPiece))

import qualified Text.Email.Validate as EmailValidate

-- | Type to represent an email address.  Newtype wrapper around
-- 'EmailValidate.EmailAddress' with additional typeclass instances.
newtype EmailAddress = EmailAddress
    { unEmailAddress :: EmailValidate.EmailAddress }
    deriving (Data, Eq, Generic, Ord, Typeable)

instance Default Constant EmailAddress (Column PGText) where
    def :: Constant EmailAddress (Column PGText)
    def = lmap (decodeUtf8With lenientDecode . toByteString) def

instance FromField EmailAddress where
    fromField :: FieldParser EmailAddress
    -- fromField :: Field -> Maybe ByteString -> Conversion EmailAddress
    fromField field Nothing = returnError UnexpectedNull field ""
    fromField field (Just email) = maybe err return $ emailAddress email
      where
        err :: Conversion EmailAddress
        err = returnError ConversionFailed field $
            "Could not convert " <> show email <> " to email address"

-- | This instance assumes 'EmailAddress' is UTF8-encoded.  See
-- 'validateFromText'.
--
-- >>> import Data.Either (isLeft)
-- >>> fmap toText $ parseUrlPiece "foo@gmail.com"
-- Right "foo@gmail.com"
-- >>> isLeft $ (parseUrlPiece "not an email address" :: Either Text EmailAddress)
-- True
instance FromHttpApiData EmailAddress where
    parseUrlPiece :: Text -> Either Text EmailAddress
    parseUrlPiece = first pack . validateFromText

-- | Parse 'EmailAddress' from JSON.
--
-- >>> import Data.Aeson (decode)
-- >>> fmap (fmap toText) (decode "[ \"foo@gmail.com \" ]" :: Maybe [EmailAddress])
-- Just ["foo@gmail.com"]
-- >>> decode "[ \"not an email address\" ]" :: Maybe [EmailAddress]
-- Nothing
instance FromJSON EmailAddress where
    parseJSON :: Value -> Parser EmailAddress
    parseJSON = withText "EmailAddress" $ \t ->
                    case validate $ encodeUtf8 t of
                        Left err -> fail $ "Failed to parse email address: " <> err
                        Right email -> return email
    {-# INLINE parseJSON #-}

-- | See 'emailAddressFromText' and 'toText'.
--
-- >>> fmap toText $ fromPathPiece "foo@gmail.com"
-- Just "foo@gmail.com"
-- >>> fmap toText $ fromPathPiece "this is not an email address"
-- Nothing
-- >>> toPathPiece $ unsafeEmailAddress "foo" "gmail.com"
-- "foo@gmail.com"
instance PathPiece EmailAddress where
    fromPathPiece :: Text -> Maybe EmailAddress
    fromPathPiece = emailAddressFromText

    toPathPiece :: EmailAddress -> Text
    toPathPiece = toText

-- | Treat 'EmailAddress' just like a 'Text' value.
--
-- >>> import Data.Either (isLeft)
-- >>> import Database.Persist.Types (PersistValue(PersistBool, PersistText))
-- >>> toPersistValue $ unsafeEmailAddress "foo" "gmail.com"
-- PersistText "foo@gmail.com"
-- >>> fmap toText $ fromPersistValue (PersistText "foo@gmail.com")
-- Right "foo@gmail.com"
-- >>> isLeft (fromPersistValue (PersistText "not an email address") :: Either Text EmailAddress)
-- True
-- >>> isLeft (fromPersistValue (PersistBool False) :: Either Text EmailAddress)
-- True
instance PersistField EmailAddress where
    toPersistValue :: EmailAddress -> PersistValue
    toPersistValue = toPersistValue . toText

    fromPersistValue :: PersistValue -> Either Text EmailAddress
    fromPersistValue = first pack . validateFromText <=< fromPersistValue

-- | Treat 'EmailAddress' just like a 'Text' value.
--
-- >>> sqlType (Proxy :: Proxy EmailAddress)
-- SqlString
instance PersistFieldSql EmailAddress where
    sqlType :: Proxy EmailAddress -> SqlType
    sqlType _ = sqlType (Proxy :: Proxy Text)

instance QueryRunnerColumnDefault PGText EmailAddress where
    queryRunnerColumnDefault :: QueryRunnerColumn PGText EmailAddress
    queryRunnerColumnDefault = fieldQueryRunnerColumn

-- |
-- >>> toText $ read "\"foo@gmail.com\""
-- "foo@gmail.com"
instance Read EmailAddress where
    readPrec :: ReadPrec EmailAddress
    readPrec = fmap EmailAddress readPrec

-- |
-- >>> show $ unsafeEmailAddress "foo" "gmail.com"
-- "\"foo@gmail.com\""
instance Show EmailAddress where
    show :: EmailAddress -> String
    show = show . unEmailAddress

-- | Turn 'EmailAddress' into JSON.
--
-- >>> toJSON $ unsafeEmailAddress "foo" "gmail.com"
-- String "foo@gmail.com"
instance ToJSON EmailAddress where
    toJSON :: EmailAddress -> Value
    toJSON = String . decodeUtf8With lenientDecode . toByteString

-- | This instance assumes 'EmailAddress' is UTF8-encoded.  See 'toText'.
--
-- >>> toUrlPiece $ unsafeEmailAddress "foo" "gmail.com"
-- "foo@gmail.com"
instance ToHttpApiData EmailAddress where
      toUrlPiece = toText

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

-- | Create an 'EmailAddress' from a 'Text' value.  See 'validate'.
validateFromText :: Text -> Either String EmailAddress
validateFromText = validate . encodeUtf8

-- | Create an 'EmailAddress' from a 'Text' value.  See 'emailAddress'.
emailAddressFromText :: Text -> Maybe EmailAddress
emailAddressFromText = emailAddress . encodeUtf8

-- | Create an 'EmailAddress' from a 'String' value.  See 'validate'.
validateFromString :: String -> Either String EmailAddress
validateFromString = validateFromText . pack

-- | Create an 'EmailAddress' from a 'String' value.  See 'emailAddress'.
emailAddressFromString :: String -> Maybe EmailAddress
emailAddressFromString = emailAddressFromText . pack

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

-- | Convert an email address to 'Text'.
--
-- This assumes the 'EmailAddress' is UTF8-encoded.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> toText email
-- "foo@gmail.com"
toText :: EmailAddress -> Text
toText = decodeUtf8With lenientDecode . toByteString
