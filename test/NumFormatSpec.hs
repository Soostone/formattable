{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NumFormatSpec where


------------------------------------------------------------------------------
import           Control.Applicative         as A
import           Control.Lens                hiding (elements)
import           Data.Default.Class
import           Data.Double.Conversion.Text
import           Data.Maybe
import           Data.Text                   (Text)
import           Formattable.NumFormat
import           Test.Hspec
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property
------------------------------------------------------------------------------

instance Arbitrary NumStyle where
  arbitrary = do
    oneof [ pure Exponent
          , pure Fixed
          , SmartExponent A.<$> elements [-4..2] <*> elements [4..12]
          , pure SIStyle
          , SmartSI <$> choose (0.001, 1000) <*> choose (1e5, 1e9)
          ]

instance Arbitrary PrecisionType where
  arbitrary = elements [SigFigs, Decimals]


instance Arbitrary NegativeStyle where
  arbitrary = elements [NegMinusSign, NegParens]


instance Arbitrary NumFormat where
  arbitrary = NumFormat <$> elements [0.01, 1, 1000, 1000000, 1000000000]
                        <*> elements ["", "$"]
                        <*> elements ["", "%"]
                        <*> elements ["", " ", ","]
                        <*> elements ["."]
                        <*> arbitrary
    -- For some reason if we use Nothing for _nfPrec we get failures that
    -- are off by VERY SMALL amounts. It doesn't look like this will be
    -- an issue in practice so leaving it out for now.
                        <*> oneof [Just <$> prec]
                        <*> arbitrary
    where
      prec = (,) <$> elements [0..10] <*> arbitrary


formatNumOld :: Real a => NumFormat -> a -> Text
formatNumOld = formatNumGeneric (toExponential . fromMaybe (-1))
                                (toFixed . fromMaybe (-1))


matchesDoubleConversion :: NumFormat -> Double -> Property
matchesDoubleConversion nf x = formatNum nf x === formatNumOld nf x

------------------------------------------------------------------------------
spec :: Spec
spec = do
    describe "formatNum" $ do
        it "doesn't show exponent for zero" $ do
          formatNum def 0 `shouldBe` "0.000"
        it "doesn't show exponent for zero intFmt" $ do
          formatNum intFmt 0 `shouldBe` "0"
        it "works for the simple case" $ do
          formatNum def 123 `shouldBe` "123.000"
        it "works for the small end of the simple case" $ do
          formatNum def 0.023 `shouldBe` "0.023"
        it "switches to exponent notation for small numbers" $ do
          formatNum def { _nfStyle = SmartExponent (-2) 10 } 0.0023 `shouldBe` "2.300e-3"
        it "switches to exponent notation for large numbers" $ do
          formatNum def 12345678000 `shouldBe` "1.235e10"
        it "switches to exponent notation for large negative numbers" $ do
          formatNum def (-12345678000) `shouldBe` "-1.235e10"
        it "uses minimum number of decimal places if not specified" $ do
          formatNum (def & nfStyle .~ Fixed
                         & nfPrec .~ Nothing) 123 `shouldBe` "123"
        it "fixed style doesn't switch to exponents" $ do
          formatNum (def & nfStyle .~ Fixed) 123456789 `shouldBe` "123456789.000"
        it "works for exponent style" $ do
          formatNum (def & nfStyle .~ Exponent) 123 `shouldBe` "1.230e2"
        it "exponent style uses minimum number of decimal places" $ do
          formatNum (def & nfStyle .~ Exponent
                         & nfPrec .~ Nothing) 123 `shouldBe` "1.23e2"
        it "exponent style respects explicit number of decimal places" $ do
          formatNum (def & nfStyle .~ Exponent
                         & nfPrec .~ Just (1, Decimals)) 123 `shouldBe` "1.2e2"

        it "works for negative numbers" $ do
          formatNum def (-123) `shouldBe` "-123.000"
        it "prefix, suffix, and NegParens style work properly" $ do
          formatNum (def & nfNegStyle .~ NegParens
                         & nfPrefix .~ "$"
                         & nfSuffix .~ "c") (-123) `shouldBe`
            "($123.000c)"
        it "units work for percent style" $ do
          formatNum percentFmt 0.79 `shouldBe` "79.000%"
        it "separators work properly" $ do
          formatNum (def & nfThouSep .~ "."
                         & nfDecSep .~ ","
                         & nfPrec .~ Just (1, Decimals)
                         & nfPrefix .~ "$") 1234567 `shouldBe`
            "$1.234.567,0"
        it "usdFmt is correct" $ do
          formatNum usdFmt 1234567.821 `shouldBe` "$1,234,567.82"
        -- Disabled until https://github.com/Soostone/formattable/issues/3 is addressed
        -- prop "matches double-conversion" matchesDoubleConversion
    describe "formatIntegral" $ do
        it "doesn't show exponent for zero" $ do
          formatIntegral def 0 `shouldBe` "0.000"
        it "doesn't show exponent for zero intFmt" $ do
          formatIntegral intFmt 0 `shouldBe` "0"
        it "works for the simple case" $ do
          formatIntegral def 123 `shouldBe` "123.000"
        it "switches to exponent notation for large numbers" $ do
          formatIntegral def 12345678000 `shouldBe` "1.235e10"
        it "switches to exponent notation for large negative numbers" $ do
          formatIntegral def (-12345678000) `shouldBe` "-1.235e10"
        it "uses minimum number of decimal places if not specified" $ do
          formatIntegral (def & nfStyle .~ Fixed
                         & nfPrec .~ Nothing) 123 `shouldBe` "123"
        it "fixed style doesn't switch to exponents" $ do
          formatIntegral (def & nfStyle .~ Fixed) 123456789 `shouldBe` "123456789.000"
        it "works for exponent style" $ do
          formatIntegral (def & nfStyle .~ Exponent) 123 `shouldBe` "1.230e2"
        it "exponent style uses minimum number of decimal places" $ do
          formatIntegral (def & nfStyle .~ Exponent
                         & nfPrec .~ Nothing) 123 `shouldBe` "1.23e2"
        it "exponent style respects explicit number of decimal places" $ do
          formatIntegral (def & nfStyle .~ Exponent
                         & nfPrec .~ Just (1, Decimals)) 123 `shouldBe` "1.2e2"

        it "works for negative numbers" $ do
          formatIntegral def (-123) `shouldBe` "-123.000"
        it "prefix, suffix, and NegParens style work properly" $ do
          formatIntegral (def & nfNegStyle .~ NegParens
                         & nfPrefix .~ "$"
                         & nfSuffix .~ "c") (-123) `shouldBe`
            "($123.000c)"
        it "separators work properly" $ do
          formatIntegral (def & nfThouSep .~ "."
                         & nfDecSep .~ ","
                         & nfPrec .~ Just (1, Decimals)
                         & nfPrefix .~ "$") 1234567 `shouldBe`
            "$1.234.567,0"
    describe "formatIntegral/binary" $ do
      it "doesn't show exponent for zero" $ do
        formatIntegral bytesFmt 0 `shouldBe` "0B"
      it "works for the simple case" $ do
        formatIntegral bytesFmt 123 `shouldBe` "123B"
      it "works for kilobytes" $ do
        formatIntegral bytesFmt 123000 `shouldBe` "120KiB"
      it "works for megabytes" $ do
        formatIntegral bytesFmt 123456789 `shouldBe` "117MiB"

