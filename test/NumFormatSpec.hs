{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NumFormatSpec where


------------------------------------------------------------------------------
import           Control.Lens
import           Data.Default.Class
import           Formattable.NumFormat
import           Test.Hspec
------------------------------------------------------------------------------


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
          formatNum def 0.0023 `shouldBe` "2.300e-3"
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
                         & nfPrec .~ Just (Decimals 1)) 123 `shouldBe` "1.2e2"

        it "works for negative numbers" $ do
          formatNum def (-123) `shouldBe` "-123.000"
        it "prefix, suffix, and NegParens style work properly" $ do
          formatNum (def & nfNegStyle .~ NegParens
                         & nfPrefix .~ Just "$"
                         & nfSuffix .~ Just "c") (-123) `shouldBe`
            "($123.000c)"
        it "units work for percent style" $ do
          formatNum percentFmt 0.79 `shouldBe` "79.000%"
        it "separators work properly" $ do
          formatNum (def & nfThouSep .~ Just '.'
                         & nfDecSep .~ ','
                         & nfPrec .~ Just (Decimals 1)
                         & nfPrefix .~ Just "$") 1234567 `shouldBe`
            "$1.234.567,0"
        it "usdFmt is correct" $ do
          formatNum usdFmt 1234567.821 `shouldBe` "$1,234,567.82"
