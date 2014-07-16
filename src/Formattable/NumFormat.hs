{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Formattable.NumFormat
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  libs@soostone.com
-- Stability   :  experimental
--
-- Formatting for numeric values.
----------------------------------------------------------------------------

module Formattable.NumFormat
    ( NumFormat(..)
    , NumStyle(..)
    , autoStyle
    , Precision(..)
    , NegativeStyle(..)

    -- * Lenses
    , nfUnits
    , nfPrefix
    , nfSuffix
    , nfThouSep
    , nfDecSep
    , nfStyle
    , nfPrec
    , nfNegStyle

    -- * Common formats
    , rawIntFmt
    , intFmt
    , percentFmt
    , numFmt
    , usdFmt

    -- * Formatting functions
    , formatPct
    , formatNum
    ) where


-------------------------------------------------------------------------------
import           Control.Lens
import           Data.Default.Class
import           Data.Double.Conversion.Text
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Typeable
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Data structure describing available styles of number formatting.
data NumStyle
    = Exponent
      -- ^ Format with scientific notation
    | Fixed
      -- ^ Format with standard decimal notation
    | SmartStyle Int Int
      -- ^ The aruments a and b define bounds.  If the absolute value of the
      -- number is in the interval [10 ^ a, 10 ^ b], then it uses the Fixed
      -- style.  If the number is outside this interval, then use the Exponent
      -- style.
  deriving (Eq,Show)


------------------------------------------------------------------------------
-- | A reasonable default value for NumStyle.
autoStyle :: NumStyle
autoStyle = SmartStyle (-2) 7


------------------------------------------------------------------------------
-- | Data structure for different methods of specifying precision.
data Precision
    = SigFigs Int
      -- ^ Specifies precision as a fixed number of significant digits
    | Decimals Int
      -- ^ Specifies precision with a fixed number of digits after the decimal
      -- place.
  deriving (Eq,Show)


------------------------------------------------------------------------------
-- | Different styles for representing negative numbers.
data NegativeStyle
    = NegMinusSign
      -- ^ Shows negative numbers as -123.000
    | NegParens
      -- ^ Shows negative numbers as (123.000)
  deriving (Eq,Show)


------------------------------------------------------------------------------
instance Default NumFormat where
    def = NumFormat 1 Nothing Nothing Nothing '.' autoStyle
                    (Just $ Decimals 3) NegMinusSign


------------------------------------------------------------------------------
-- The main data structure with all the necessary information for formatting
-- numbers.
data NumFormat = NumFormat
    { _nfUnits    :: Double
      -- ^ Units of measure to use in formatting the number.  This is useful
      -- for things like percentages where you would use (units of 0.01) or
      -- financial statements (units of 1000 for example).
    , _nfPrefix   :: Maybe Text
      -- ^ A prefix to add to the number, commonly used for currency
      -- designation.
    , _nfSuffix   :: Maybe Text
      -- ^ A suffix for the number.  Percent, for example.
    , _nfThouSep  :: Maybe Char
      -- ^ The character to use as thousands separator if applicable.
    , _nfDecSep   :: Char
      -- ^ Character to use for the decimal separator.
    , _nfStyle    :: NumStyle
      -- ^ The formatting style
    , _nfPrec     :: Maybe Precision
      -- ^ Amount of precision to display
    , _nfNegStyle :: NegativeStyle
      -- ^ Styles for negative numbers
    } deriving (Eq,Show,Typeable)
makeLenses ''NumFormat


------------------------------------------------------------------------------
-- | A type wrapper to make it easier to handle the pipeline of
-- transformations that happen after we have split the number at the decimal
-- place.
data RawNum = RawNum
    { rawNumX :: Double
      -- ^ The original number so we can check for zero
    , rawNumN :: Text
      -- ^ Integral part (left of the decimal place)
    , rawNumD :: Text
      -- ^ Decimal part (right of the decimal place)
    , rawNumE :: Text
      -- ^ Exponent part if there is one
    }


------------------------------------------------------------------------------
-- | Take the raw output from the double-precision conversion functions and
-- split into individual components.
--
-- We need to split on dot because that's what double-conversion uses as the
-- decimal separator.
mkRawNum :: Real a => a -> Text -> RawNum
mkRawNum x t = RawNum (realToFrac x) n d (T.drop 1 e)
  where
    (n,rest) = T.span (/= '.') t
    (d,e) = T.span (/= 'e') (T.drop 1 rest)


-------------------------------------------------------------------------------
-- | Int format with no thousands separator.
rawIntFmt :: NumFormat
rawIntFmt = def & nfPrec .~ Just (Decimals 0)


-------------------------------------------------------------------------------
-- | Int format with comma as the thousands separator.
intFmt :: NumFormat
intFmt = def & nfPrec .~ Just (Decimals 0)
             & nfThouSep .~ Just ','


------------------------------------------------------------------------------
-- | Common format for percentages.  Example: 75.000%
percentFmt :: NumFormat
percentFmt = def & nfSuffix .~ Just "%"
                 & nfUnits .~ 0.01


------------------------------------------------------------------------------
-- | Common format for generic numeric quantities of the form 123,456.99.
numFmt :: NumFormat
numFmt = def & nfThouSep .~ Just ','
             & nfPrec .~ Just (Decimals 2)


------------------------------------------------------------------------------
-- | Common format for US dollar quantities of the form $123,456.99.
usdFmt :: NumFormat
usdFmt = def & nfPrefix .~ Just "$"
             & nfThouSep .~ Just ','
             & nfPrec .~ Just (Decimals 2)
             & nfStyle .~ Fixed


-------------------------------------------------------------------------------
-- | Convenience wrapper for percentages that lets you easily control the
-- number of decimal places.
formatPct :: Real a => Int -> a -> Text
formatPct p = formatNum (percentFmt & nfPrec .~ Just (Decimals p))


-------------------------------------------------------------------------------
-- | This function checks to see if the number is smaller than the number of
-- digits of precision being displayed and if so, switches to scientific
-- notation.
formatNum :: Real a => NumFormat -> a -> Text
formatNum NumFormat{..} noUnits =
    whenNegative noUnits (addSign _nfNegStyle) $ addPrefix $ addSuffix $
    addDecimal _nfDecSep $ maybe id (addThousands . T.singleton) _nfThouSep $
    maybe id limitPrecision _nfPrec $ mkRawNum noUnits formatted
  where
    a = abs $ realToFrac noUnits / _nfUnits
    formatted = case _nfStyle of
                Exponent -> toExponential precArg a
                Fixed -> toFixed precArg a
                SmartStyle lo hi -> smartStyle lo hi precArg a
    addPrefix x = maybe x (<> x) _nfPrefix
    addSuffix x = maybe x (x <>) _nfSuffix
    precArg = maybe (-1) p _nfPrec
      where
        p (SigFigs n) = n
        p (Decimals n) = n


------------------------------------------------------------------------------
-- | A "pre-format" function that intelligently chooses between fixed and
-- exponential format
smartStyle :: Int -> Int -> Int -> Double -> Text
smartStyle l h precArg x =
    if lo < x' && x < hi
      then toFixed precArg x
      else toExponential precArg x
  where
    x' = abs x
    lo = 10.0 ** fromIntegral l
    hi = 10.0 ** fromIntegral h


------------------------------------------------------------------------------
limitPrecision :: Precision -> RawNum -> RawNum
limitPrecision p r@(RawNum x n d e) =
    case p of
      SigFigs c ->
        if c < T.length n
          then RawNum x (T.take c n <> T.replicate (T.length n - c) "0") "" e
          else RawNum x n (T.take (c - T.length n) d) e
      Decimals c -> if c == (-1) then r else RawNum x n (T.take c d) e


------------------------------------------------------------------------------
whenNegative :: (Num n, Ord n) => n -> (a -> a) -> a -> a
whenNegative n f = if n < 0 then f else id


------------------------------------------------------------------------------
addSign :: NegativeStyle -> Text -> Text
addSign NegMinusSign t = T.cons '-' t
addSign NegParens t = T.concat ["(", t, ")"]


-------------------------------------------------------------------------------
addThousands :: Text -> RawNum -> RawNum
addThousands sep (RawNum x n d e) = RawNum x n' d e
  where
    n' = T.reverse . T.intercalate sep . T.chunksOf 3 . T.reverse $ n


------------------------------------------------------------------------------
addDecimal :: Char -> RawNum -> Text
addDecimal c (RawNum x n d e) = T.concat [n, d', e']
  where
    d' = if T.null d then "" else T.cons c d
    e' = if T.null e || x == 0 then "" else T.cons 'e' e


