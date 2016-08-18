{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

module Text.Email.Validate.Internal
    ( EmailAddress(unEmailAddress)
    , EmailValidate.isValid
    , validate
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
