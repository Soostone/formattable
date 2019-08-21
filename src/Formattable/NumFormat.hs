{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
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
    (
    -- * Main number formatting functions and data types
      formatNum
    , formatIntegral
    , formatPct
    , NumFormat(..)
    , NumStyle(..)
    , autoStyle
    , PrecisionType(..)
    , NegativeStyle(..)

    -- * Common formats
    , rawIntFmt
    , intFmt
    , percentFmt
    , numFmt
    , usdFmt

    -- * Lenses
    , nfUnits
    , nfPrefix
    , nfSuffix
    , nfThouSep
    , nfDecSep
    , nfStyle
    , nfPrec
    , nfNegStyle

    -- * Other formatting functions
    , formatNumGeneric
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative as A
import           Data.Char
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid         as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable
import           Numeric
-------------------------------------------------------------------------------


type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s A.<$> afb (sa s)



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
      -- ^ Adds the symbol for the closest smaller SI prefix as the suffix to
      -- the formatted number.  This suffix appears before any other suffix
      -- you designate.
    | SmartSI Double Double
      -- ^ Like SIStyle but only applies the SI prefix if the number to be
      -- formatted falls within the range [a,b] given by the SmartSI
      -- arguments.
  deriving (Eq,Show)


------------------------------------------------------------------------------
-- | A reasonable default value for NumStyle.
--
-- @SmartExponent (-6) 10@
autoStyle :: NumStyle
autoStyle = SmartExponent (-6) 10


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
-- | The main data structure with all the necessary information for formatting
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

nfUnits :: Lens NumFormat NumFormat Double Double
nfUnits = lens _nfUnits setter
  where
    setter sc v = sc { _nfUnits = v }

nfPrefix :: Lens NumFormat NumFormat Text Text
nfPrefix = lens _nfPrefix setter
  where
    setter sc v = sc { _nfPrefix = v }

nfSuffix :: Lens NumFormat NumFormat Text Text
nfSuffix = lens _nfSuffix setter
  where
    setter sc v = sc { _nfSuffix = v }

nfThouSep :: Lens NumFormat NumFormat Text Text
nfThouSep = lens _nfThouSep setter
  where
    setter sc v = sc { _nfThouSep = v }

nfDecSep :: Lens NumFormat NumFormat Text Text
nfDecSep = lens _nfDecSep setter
  where
    setter sc v = sc { _nfDecSep = v }

nfStyle :: Lens NumFormat NumFormat NumStyle NumStyle
nfStyle = lens _nfStyle setter
  where
    setter sc v = sc { _nfStyle = v }

nfPrec :: Lens NumFormat NumFormat (Maybe (Int, PrecisionType)) (Maybe (Int, PrecisionType))
nfPrec = lens _nfPrec setter
  where
    setter sc v = sc { _nfPrec = v }

nfNegStyle :: Lens NumFormat NumFormat NegativeStyle NegativeStyle
nfNegStyle = lens _nfNegStyle setter
  where
    setter sc v = sc { _nfNegStyle = v }


------------------------------------------------------------------------------
instance Default NumFormat where
    def = NumFormat
      { _nfUnits = 1
      , _nfPrefix = ""
      , _nfSuffix = ""
      , _nfThouSep = ""
      , _nfDecSep = "."
      , _nfStyle = autoStyle
      , _nfPrec = Just (3, Decimals)
      , _nfNegStyle = NegMinusSign
      }


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
  deriving (Eq,Show)


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
rawIntFmt = def { _nfPrec = Just (0, Decimals) }


-------------------------------------------------------------------------------
-- | Int format with comma as the thousands separator.
intFmt :: NumFormat
intFmt = def { _nfPrec = Just (0, Decimals)
             , _nfThouSep = ","
             }


------------------------------------------------------------------------------
-- | Common format for percentages.  Example: 75.000%
percentFmt :: NumFormat
percentFmt = def { _nfSuffix = "%"
                 , _nfUnits = 0.01
                 }


------------------------------------------------------------------------------
-- | Common format for generic numeric quantities of the form 123,456.99.
numFmt :: NumFormat
numFmt = def { _nfThouSep = ","
             , _nfPrec = Just (2, Decimals)
             }


------------------------------------------------------------------------------
-- | Common format for US dollar quantities of the form $123,456.99.
usdFmt :: NumFormat
usdFmt = def { _nfPrefix = "$"
             , _nfThouSep = ","
             , _nfPrec = Just (2, Decimals)
             , _nfStyle = Fixed
             }


-------------------------------------------------------------------------------
-- | Convenience wrapper for percentages that lets you easily control the
-- number of decimal places.
formatPct :: Real a => Int -> a -> Text
formatPct p = formatNum (percentFmt { _nfPrec = Just (p, Decimals) })


-------------------------------------------------------------------------------
-- | Primary function for formatting integrals.  This was originally created to
-- avoid depending on the double-conversion package which uses a C library and
-- is therefore less portable.  We're keeping this as a separate function
-- because it should have the potential to be more efficient than the floating
-- point version.
formatIntegral :: Integral a => NumFormat -> a -> Text
formatIntegral NumFormat{..} noUnits =
    whenNegative noUnits (addSign _nfNegStyle) $
    addPrefix $
    addSuffix $
    addDecimal _nfDecSep $
    addThousands _nfThouSep $
    maybe id limitPrecision _nfPrec $
    formatted siUnitized
  where
    a = abs $ noUnits `div` round _nfUnits
    formatted = case _nfStyle of
                  Exponent -> exponentialInt precArg
                  Fixed -> fixedInt precArg
                  SmartExponent lo hi -> smartStyleIntegral lo hi
                                           (fixedInt precArg)
                                           (exponentialInt precArg)
                  SIStyle -> fixedInt precArg
                  SmartSI _ _ -> fixedInt precArg
    addPrefix x = _nfPrefix M.<> x
    addSuffix x1 = let x2 = x1 <> siSuffix in x2 <> _nfSuffix
    precArg = maybe (-1) fst _nfPrec
    (e, siSuffix) = case _nfStyle of
                      SIStyle -> siPrefixIntegral a
                      SmartSI lo hi -> if fromIntegral a > lo && fromIntegral a < hi then siPrefixIntegral a else (0, "")
                      _ -> (0, "")
    siUnitized = a `div` 10^e


-- The following two functions skip the double-conversion library and let us
-- go straight to RawNum when we're working with Integrals.  This avoids a C
-- dependency and hopefully should also be faster.


fixedInt :: Integral a => Int -> a -> RawNum a
fixedInt (-1) n =
    RawNum n (T.pack $ showIntegralBase 10 n) "" ""
fixedInt decimals n =
    RawNum n (T.pack $ showIntegralBase 10 n) (T.replicate decimals "0") ""


exponentialInt :: Integral a => Int -> a -> RawNum a
exponentialInt (-1) n = RawNum n (T.pack a) (T.pack b) (T.pack $ show (len-1))
  where
    str = showIntegralBase 10 n
    len = length str
    (a,b) = splitAt 1 str
exponentialInt numDecimals n =
    RawNum n (T.pack a) (T.pack b) (T.pack $ show e)
  where
    str = showIntegralBase 10 n
    len = length str
    (keep,junk) = splitAt (numDecimals+1) (str ++ repeat '0')
    rounded = if head junk >= '5'
                then reverse $ roundStr $ reverse keep
                else keep
    e = if length rounded > length keep then len else len-1
    (a,b) = splitAt 1 rounded

    roundStr [] = "1"
    roundStr (x:xs) = if x == '9' then '0' : roundStr xs else succ x : xs


-------------------------------------------------------------------------------
-- | Primary function for formatting floating point numbers.
formatNum :: Real a => NumFormat -> a -> Text
formatNum = formatNumGeneric (\p x -> T.pack $ showEFloat p x "")
                             (\p x -> T.pack $ showFFloat p x "")


-------------------------------------------------------------------------------
-- | Generic floating point formatting function that allows you to specify your
-- own underlying functions for formatting exponential and fixed formats.  This
-- can allow you to use more efficient versions if available.  We also use it
-- the test suite to check behavior against the old double-conversion
-- implementation.
formatNumGeneric
    :: Real a
    => (Maybe Int -> Double -> Text)
    -- ^ Exponential formatter
    -> (Maybe Int -> Double -> Text)
    -- ^ Fixed formatter
    -> NumFormat
    -- ^ Format specification
    -> a
    -- ^ The number to format
    -> Text
formatNumGeneric fmtExp fmtFixed NumFormat{..} noUnits =
    whenNegative noUnits (addSign _nfNegStyle) $
    addPrefix $
    addSuffix $
    addDecimal _nfDecSep $
    addThousands _nfThouSep $
    maybe id limitPrecision _nfPrec $
    stripZeros precArg $
    mkRawNum noUnits $
    formatted siUnitized
  where
    a = abs $ realToFrac noUnits / _nfUnits
    formatted = case _nfStyle of
                  Exponent -> fmtExp precArg
                  Fixed -> fmtFixed precArg
                  SmartExponent lo hi -> smartStyle lo hi (fmtFixed precArg) (fmtExp precArg)
                  SIStyle -> fmtFixed precArg
                  SmartSI _ _ -> fmtFixed precArg
    addPrefix x = _nfPrefix <> x
    addSuffix x1 = let x2 = x1 <> siSuffix in x2 <> _nfSuffix
    precArg = fst <$> _nfPrec
    (e, siSuffix) = case _nfStyle of
                      SIStyle -> siPrefix a
                      SmartSI lo hi -> if a > lo && a < hi then siPrefix a else (0, "")
                      _ -> (0, "")
    siUnitized = a / 10**(fromIntegral e)


stripZeros :: Maybe Int -> RawNum a -> RawNum a
stripZeros precArg rn@(RawNum x a b c) =
  if isNothing precArg && T.all (=='0') b
    then RawNum x a "" c
    else rn


siPrefixIntegral :: Integral a => a -> (Int, Text)
siPrefixIntegral x
  | abs x > 10^(24::Int) = (24, "Y")
  | abs x > 10^(21::Int) = (21, "Z")
  | abs x > 10^(18::Int) = (18, "E")
  | abs x > 10^(15::Int) = (15, "P")
  | abs x > 10^(12::Int) = (12, "T")
  | abs x > 10^(9::Int) = (9, "G")
  | abs x > 10^(6::Int) = (6, "M")
  | abs x > 10^(3::Int) = (3, "k")
  | otherwise = (0, "")


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
smartStyleIntegral
    :: (Num a, Ord a)
    => Int
    -- ^ Lower bound exponent
    -> Int
    -- ^ Upper bound exponent
    -> (a -> b)
    -- ^ Function to call if within range
    -> (a -> b)
    -- ^ Function to call if out of range
    -> a
    -> b
smartStyleIntegral l h f g x =
    if lo < x' && x' < hi
      then f x
      else g x
  where
    x' = abs x
    lo = 10 ^ (max l 0)
    hi = 10 ^ (max h 0)


------------------------------------------------------------------------------
-- | A "pre-format" function that intelligently chooses between fixed and
-- exponential format
smartStyle
    :: (Floating a, Ord a)
    => Int
    -- ^ Lower bound exponent
    -> Int
    -- ^ Upper bound exponent
    -> (a -> b)
    -- ^ Function to call if within range
    -> (a -> b)
    -- ^ Function to call if out of range
    -> a
    -> b
smartStyle l h f g x =
    if lo < x' && x' < hi
      then f x
      else g x
  where
    x' = abs x
    lo = 10 ** fromIntegral l
    hi = 10 ** fromIntegral h


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


------------------------------------------------------------------------------
-- | An integral show function without the Show constraint.  It works for
-- bases in the range [0,36].
showIntegralBase
    :: Integral a
    => a
    -- ^ The base
    -> a
    -- ^ The number to convert to string
    -> String
showIntegralBase b n
    -- It's ok to negate b because we only support bases in the range [0,36]
  | n < b && n > negate b = go n ""
    -- We unroll the go loop once because you can't negate minBound
  | otherwise =
    let (q,r) = n `quotRem` b in go q (go (abs r) "")
  where
    go m str
      | m < 0 = '-' : go (-m) str
      | m < b = integralDigit m : str
      | otherwise = let (q,r) = m `quotRem` b
                     in go q (integralDigit r : str)


------------------------------------------------------------------------------
integralDigit :: Integral a => a -> Char
integralDigit n
  | n < 10 = chr $ ord '0' + fromIntegral n
  | n < 36 = chr $ ord 'a' + fromIntegral n - 10
  | otherwise = error "integralDigit: not a digit"


