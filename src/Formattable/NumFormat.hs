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
    , PrecisionType(..)
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
    | SmartExponent Int Int
      -- ^ The aruments a and b define bounds.  If the absolute value of the
      -- number is in the interval [10 ^ a, 10 ^ b], then it uses the Fixed
      -- style.  If the number is outside this interval, then use the Exponent
      -- style.
    | SIStyle
    | SmartSI Double Double
  deriving (Eq,Show)


------------------------------------------------------------------------------
-- | A reasonable default value for NumStyle.
autoStyle :: NumStyle
autoStyle = SmartExponent (-2) 10


------------------------------------------------------------------------------
-- | Data structure for different methods of specifying precision.
data PrecisionType
    = SigFigs
      -- ^ Specifies precision as a fixed number of significant digits
    | Decimals
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
-- The main data structure with all the necessary information for formatting
-- numbers.
data NumFormat = NumFormat
    { _nfUnits    :: Double
      -- ^ Units of measure to use in formatting the number.  This is useful
      -- for things like percentages where you would use (units of 0.01) or
      -- financial statements (units of 1000 for example).
    , _nfPrefix   :: Text
      -- ^ A prefix to add to the number, commonly used for currency
      -- designation.
    , _nfSuffix   :: Text
      -- ^ A suffix for the number.  Percent, for example.
    , _nfThouSep  :: Text
      -- ^ The character to use as thousands separator if applicable.
    , _nfDecSep   :: Text
      -- ^ Character to use for the decimal separator.
    , _nfStyle    :: NumStyle
      -- ^ The formatting style
    , _nfPrec     :: Maybe (Int, PrecisionType)
      -- ^ Amount of precision to display
    , _nfNegStyle :: NegativeStyle
      -- ^ Styles for negative numbers
    } deriving (Eq,Show,Typeable)
makeLenses ''NumFormat


------------------------------------------------------------------------------
instance Default NumFormat where
    def = NumFormat 1 "" "" "" "." autoStyle
                    (Just $ (3, Decimals)) NegMinusSign


------------------------------------------------------------------------------
-- | A type wrapper to make it easier to handle the pipeline of
-- transformations that happen after we have split the number at the decimal
-- place.
data RawNum a = RawNum a Text Text Text
--    { rawNumX :: a
--      -- ^ The original number so we can check for zero
--    , rawNumN :: Text
--      -- ^ Integral part (left of the decimal place)
--    , rawNumD :: Text
--      -- ^ Decimal part (right of the decimal place)
--    , rawNumE :: Text
--      -- ^ Exponent part if there is one
--    }


------------------------------------------------------------------------------
-- | Take the raw output from the double-precision conversion functions and
-- split into individual components.
--
-- We need to split on dot because that's what double-conversion uses as the
-- decimal separator.
mkRawNum :: Real a => a -> Text -> RawNum a
mkRawNum x t =
    case (T.findIndex (== '.') t, T.findIndex (== 'e') t) of
      (Nothing, Nothing) -> mk t "" ""
      (Just i, Nothing) -> let (n,d) = T.splitAt i t
                            in mk n (T.drop 1 d) ""
      (Nothing, Just i) -> let (n,e) = T.splitAt i t
                            in mk n "" (T.drop 1 e)
      (Just i, Just j) -> let (n,rest) = T.splitAt i t
                              (d,e) = T.splitAt (j-i-1) (T.drop 1 rest)
                           in mk n d (T.drop 1 e)
  where
    mk = RawNum x


-------------------------------------------------------------------------------
-- | Int format with no thousands separator.
rawIntFmt :: NumFormat
rawIntFmt = def & nfPrec .~ Just (0, Decimals)


-------------------------------------------------------------------------------
-- | Int format with comma as the thousands separator.
intFmt :: NumFormat
intFmt = def & nfPrec .~ Just (0, Decimals)
             & nfThouSep .~ ","


------------------------------------------------------------------------------
-- | Common format for percentages.  Example: 75.000%
percentFmt :: NumFormat
percentFmt = def & nfSuffix .~ "%"
                 & nfUnits .~ 0.01


------------------------------------------------------------------------------
-- | Common format for generic numeric quantities of the form 123,456.99.
numFmt :: NumFormat
numFmt = def & nfThouSep .~ ","
             & nfPrec .~ Just (2, Decimals)


------------------------------------------------------------------------------
-- | Common format for US dollar quantities of the form $123,456.99.
usdFmt :: NumFormat
usdFmt = def & nfPrefix .~ "$"
             & nfThouSep .~ ","
             & nfPrec .~ Just (2, Decimals)
             & nfStyle .~ Fixed


-------------------------------------------------------------------------------
-- | Convenience wrapper for percentages that lets you easily control the
-- number of decimal places.
formatPct :: Real a => Int -> a -> Text
formatPct p = formatNum (percentFmt & nfPrec .~ Just (p, Decimals))


-------------------------------------------------------------------------------
-- | This function checks to see if the number is smaller than the number of
-- digits of precision being displayed and if so, switches to scientific
-- notation.
formatNum :: Real a => NumFormat -> a -> Text
formatNum NumFormat{..} noUnits =
    whenNegative noUnits (addSign _nfNegStyle) $
    addPrefix $
    addSuffix $
    addDecimal _nfDecSep $
    addThousands _nfThouSep $
    maybe id limitPrecision _nfPrec $
    mkRawNum noUnits $
    formatted siUnitized

  where
    a = abs $ realToFrac noUnits / _nfUnits
    formatted = case _nfStyle of
                  Exponent -> toExponential precArg
                  Fixed -> toFixed precArg
                  SmartExponent lo hi -> smartStyle lo hi precArg
                  SIStyle -> toFixed precArg
                  SmartSI _ _ -> toFixed precArg
    addPrefix x = _nfPrefix <> x
    addSuffix x1 = let x2 = x1 <> siSuffix in x2 <> _nfSuffix
    precArg = maybe (-1) fst _nfPrec
    (e, siSuffix) = case _nfStyle of
                      SIStyle -> siPrefix a
                      SmartSI lo hi -> if a > lo && a < hi then siPrefix a else (0, "")
                      _ -> (0, "")
    siUnitized = a / 10**(fromIntegral e)


siPrefix :: Double -> (Int, Text)
siPrefix x
  | abs x > 1e24 = (24, "Y")
  | abs x > 1e21 = (21, "Z")
  | abs x > 1e18 = (18, "E")
  | abs x > 1e15 = (15, "P")
  | abs x > 1e12 = (12, "T")
  | abs x > 1e9 = (9, "G")
  | abs x > 1e6 = (6, "M")
  | abs x > 1e3 = (3, "k")
  | abs x > 1 = (0, "")
  | abs x > (1.0 / 1e3) = (-3, "m")
  | abs x > (1.0 / 1e6) = (-6, "μ")
  | abs x > (1.0 / 1e9) = (-9, "n")
  | abs x > (1.0 / 1e12) = (-12, "p")
  | abs x > (1.0 / 1e15) = (-15, "f")
  | abs x > (1.0 / 1e18) = (-18, "a")
  | abs x > (1.0 / 1e21) = (-21, "z")
  | otherwise = (-24, "y")


------------------------------------------------------------------------------
-- | A "pre-format" function that intelligently chooses between fixed and
-- exponential format
smartStyle :: Int -> Int -> Int -> Double -> Text
smartStyle l h precArg x =
    if lo < x' && x' < hi
      then toFixed precArg x
      else toExponential precArg x
  where
    x' = abs x
    lo = 10.0 ** fromIntegral l
    hi = 10.0 ** fromIntegral h


------------------------------------------------------------------------------
limitPrecision :: (Int, PrecisionType) -> RawNum a -> RawNum a
limitPrecision (c,p) r@(RawNum x n d e) =
    case p of
      SigFigs ->
        if c < T.length n
          then RawNum x (T.take c n <> T.replicate (T.length n - c) "0") "" e
          else RawNum x n (T.take (c - T.length n) d) e
      Decimals -> if c == (-1) then r else RawNum x n (T.take c d) e


------------------------------------------------------------------------------
whenNegative :: (Num n, Ord n) => n -> (a -> a) -> a -> a
whenNegative n f = if n < 0 then f else id


------------------------------------------------------------------------------
addSign :: NegativeStyle -> Text -> Text
addSign NegMinusSign t = T.cons '-' t
addSign NegParens t = T.concat ["(", t, ")"]


-------------------------------------------------------------------------------
addThousands :: Text -> RawNum a -> RawNum a
addThousands "" raw = raw
addThousands sep (RawNum x n d e) = RawNum x n' d e
  where
    n' = T.reverse . T.intercalate sep . T.chunksOf 3 . T.reverse $ n


------------------------------------------------------------------------------
addDecimal :: (Eq a, Num a) => Text -> RawNum a -> Text
addDecimal t (RawNum x n d e) = T.concat [n, d', e']
  where
    d' = if T.null d then "" else T.append t d
    e' = if T.null e || x == 0 then "" else T.cons 'e' e


