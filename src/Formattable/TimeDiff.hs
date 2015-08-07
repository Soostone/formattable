{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Formattable.NumFormat
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  libs@soostone.com
-- Stability   :  experimental
--
-- English formatting for time differences.
------------------------------------------------------------------------------

module Formattable.TimeDiff where

------------------------------------------------------------------------------
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
aYear, aMonth, aWeek, aDay, anHour, aMinute, aSecond, aMillisecond
  :: NominalDiffTime
aYear = 365 * aDay
aMonth = aYear / 12
aWeek = 7 * aDay
aDay = 24 * anHour
anHour = 60 * aMinute
aMinute = 60 * aSecond
aSecond = 1
aMillisecond = 0.001


------------------------------------------------------------------------------
englishRelativeNow :: UTCTime -> IO Text
englishRelativeNow then_ = do
    now <- getCurrentTime
    return $ englishRelative now then_


------------------------------------------------------------------------------
englishRelative :: UTCTime -> UTCTime -> Text
englishRelative now then_ =
    if d  > aYear + aMonth
      then T.pack $ formatTime defaultTimeLocale "%F" then_
      else englishDiff d
  where
    d = diffUTCTime now then_


------------------------------------------------------------------------------
englishDiff :: NominalDiffTime -> Text
englishDiff d =
    case smaller of
      [] -> "just now"
      (t,str):_ -> let n = round (abs d / t) :: Integer
                    in addInAgo $
                         T.unwords [T.pack $ show n, str] <>
                         if n >= 2 then "s" else ""
  where
    addInAgo = if d > 0 then (<>" ago") else ("in "<>)
    smaller = dropWhile ((> abs d) . fst) durations
    durations = [ (aYear, "year")
                , (aMonth, "month")
                , (aWeek, "week")
                , (aDay, "day")
                , (anHour, "hour")
                , (aMinute, "minute")
                , (aSecond, "second")
                ]
