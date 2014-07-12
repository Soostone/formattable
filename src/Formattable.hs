{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Formattable
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  libs@soostone.com
-- Stability   :  experimental
--
-- Practical formatting interface for output values intended for human
-- consumption. We try to support several variants often required by
-- real-world applications.
----------------------------------------------------------------------------

module Formattable
    ( module Formattable
    , module Formattable.NumFormat
    ) where

-------------------------------------------------------------------------------
import           Data.Int
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time
import           Data.Word
import           System.Locale
-------------------------------------------------------------------------------
import           Formattable.NumFormat
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
class Format a where

    -- | A format settings type specific to the object type.
    type TheFormat a

    -- | Render into text using format settings
    runFormat :: TheFormat a -> a -> Text


instance Format Integer where
    type TheFormat Integer = NumFormat
    runFormat set a = formatNum set a


instance Format Int where
    type TheFormat Int = NumFormat
    runFormat set a = formatNum set a


instance Format Int8 where
    type TheFormat Int8 = NumFormat
    runFormat set a = formatNum set a


instance Format Int16 where
    type TheFormat Int16 = NumFormat
    runFormat set a = formatNum set a


instance Format Int32 where
    type TheFormat Int32 = NumFormat
    runFormat set a = formatNum set a


instance Format Int64 where
    type TheFormat Int64 = NumFormat
    runFormat set a = formatNum set a


instance Format Word where
    type TheFormat Word = NumFormat
    runFormat set a = formatNum set a


instance Format Word8 where
    type TheFormat Word8 = NumFormat
    runFormat set a = formatNum set a


instance Format Word16 where
    type TheFormat Word16 = NumFormat
    runFormat set a = formatNum set a


instance Format Word32 where
    type TheFormat Word32 = NumFormat
    runFormat set a = formatNum set a


instance Format Word64 where
    type TheFormat Word64 = NumFormat
    runFormat set a = formatNum set a


instance Format Double where
    type TheFormat Double = NumFormat
    runFormat set a = formatNum set a


instance Format Float where
    type TheFormat Float = NumFormat
    runFormat set a = formatNum set a


instance Format Day where
    type TheFormat Day = String
    runFormat set a = T.pack $ formatTime defaultTimeLocale set a


instance Format UTCTime where
    type TheFormat UTCTime = String
    runFormat set a = T.pack $ formatTime defaultTimeLocale set a


instance Format Text where
    type TheFormat Text = ()
    runFormat _ a = a


instance Format Bool where
    type TheFormat Bool = ()
    runFormat _ a = T.pack (show a)
