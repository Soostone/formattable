{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Formattable.NumFormat
  ( NumStyle(..)
  , autoStyle
  , Precision(..)
  , NegativeStyle(..)
  , NumFormat(..)

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
  , usdFmt

  -- * Formatting functions
  , formatPct
  , formatCur
  , formatNum
  ) where


-------------------------------------------------------------------------------
import           Control.Lens
import           Data.Default
import           Data.Double.Conversion.Text
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Typeable
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
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
autoStyle :: NumStyle
autoStyle = SmartStyle (-2) 7


------------------------------------------------------------------------------
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
    | NegParens
  deriving (Eq,Show)


------------------------------------------------------------------------------
data NumFormat = NumFormat
    { _nfUnits    :: Double
    , _nfPrefix   :: Maybe Text
    , _nfSuffix   :: Maybe Text
    , _nfThouSep  :: Maybe Char
    , _nfDecSep   :: Char
    , _nfStyle    :: NumStyle
    , _nfPrec     :: Maybe Precision
    , _nfNegStyle :: NegativeStyle
    } deriving (Eq,Show,Typeable)
makeLenses ''NumFormat


------------------------------------------------------------------------------
instance Default NumFormat where
    def = NumFormat 1 Nothing Nothing Nothing '.' autoStyle
                    (Just $ Decimals 3) NegMinusSign


------------------------------------------------------------------------------
-- | A type wrapper to make it easier to handle the pipeline of
-- transformations that happen after we have split the number at the decimal
-- place.
data RawNum = RawNum
    { rawNumN :: Text
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
mkRawNum :: Text -> RawNum
mkRawNum t = RawNum n d (T.drop 1 e)
  where
    (n,rest) = T.span (/= '.') t
    (d,e) = T.span (/= 'e') (T.drop 1 rest)


-------------------------------------------------------------------------------
-- | Int format with no thousands separator.
rawIntFmt :: NumFormat
rawIntFmt = def & nfPrec .~ Just (Decimals 0)


-------------------------------------------------------------------------------
intFmt :: NumFormat
intFmt = def & nfPrec .~ Just (Decimals 0)
             & nfThouSep .~ Just ','


------------------------------------------------------------------------------
percentFmt :: NumFormat
percentFmt = def & nfSuffix .~ Just "%"
                 & nfUnits .~ 0.01


------------------------------------------------------------------------------
usdFmt :: NumFormat
usdFmt = def & nfPrefix .~ Just "$"
             & nfThouSep .~ Just ','
             & nfPrec .~ Just (Decimals 2)


-------------------------------------------------------------------------------
formatPct :: (Num a, Real a, Fractional a) => Int -> a -> Text
formatPct p = formatNum (percentFmt & nfPrec .~ Just (Decimals p))


-------------------------------------------------------------------------------
formatCur :: (Num a, Real a, Fractional a) => Text -> Int -> a -> Text
formatCur cur p = formatNum f
  where
    f = def & nfPrefix .~ Just cur
            & nfPrec .~ Just (Decimals p)
            & nfThouSep .~ Just ','


-------------------------------------------------------------------------------
-- | This function checks to see if the number is smaller than the number of
-- digits of precision being displayed and if so, switches to scientific
-- notation.
formatNum :: Real a => NumFormat -> a -> Text
formatNum NumFormat{..} noUnits =
    whenNegative noUnits (addSign _nfNegStyle) $ addPrefix $ addSuffix $
    addDecimal _nfDecSep $ maybe id (addThousands . T.singleton) _nfThouSep $
    maybe id limitPrecision _nfPrec $ mkRawNum formatted
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
limitPrecision p r@(RawNum n d e) =
    case p of
      SigFigs c ->
        if c < T.length n
          then RawNum (T.take c n <> T.replicate (T.length n - c) "0") "" e
          else RawNum n (T.take (c - T.length n) d) e
      Decimals c -> if c == (-1) then r else RawNum n (T.take c d) e


------------------------------------------------------------------------------
whenNegative :: (Num n, Ord n) => n -> (a -> a) -> a -> a
whenNegative n f = if n < 0 then f else id


------------------------------------------------------------------------------
addSign :: NegativeStyle -> Text -> Text
addSign NegMinusSign t = T.cons '-' t
addSign NegParens t = T.concat ["(", t, ")"]


-------------------------------------------------------------------------------
addThousands :: Text -> RawNum -> RawNum
addThousands sep (RawNum n d e) = RawNum n' d e
  where
    n' = T.reverse . T.intercalate sep . T.chunksOf 3 . T.reverse $ n


------------------------------------------------------------------------------
addDecimal :: Char -> RawNum -> Text
addDecimal c (RawNum n d e) = T.concat [n, d', e']
  where
    d' = if T.null d then "" else T.cons c d
    e' = if T.null e then "" else T.cons 'e' e


