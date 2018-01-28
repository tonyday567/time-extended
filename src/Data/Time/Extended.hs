{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-orphans#-}
{-# OPTIONS_GHC-fno-warn-type-defaults#-}

module Data.Time.Extended (module Data.Time.Extended, module Data.Time) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as AC
import Data.Binary
import Data.ByteString.Builder as B
import Data.Char (digitToInt)
import Data.Fixed (Pico)
import Data.Monoid
import Data.Text (Text)
import Data.Time
import Formatting
import Formatting.Time

-- Binary instances hard-coding millisec resolution
instance Binary UTCTime where
  put t = do
    let d = utctDay t
        s = utctDayTime t
    put d >> put s
  get = do
    d <- get
    s <- get
    return $ UTCTime d s

instance Binary DiffTime where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary Day where
  put (ModifiedJulianDay d) = put d
  get = ModifiedJulianDay <$> (get :: Get Integer)

instance Binary TimeOfDay where
  put (TimeOfDay h m s) = put h >> put m >> put s
  get = liftM3 TimeOfDay (get :: Get Int) (get :: Get Int) (get :: Get Pico)

{-
instance Binary Pico where
  put = put . fromEnum
  get = toEnum <$> (get :: Get Int)
-}

{- $UTCTime builders -}
-- 2013-10-16
bDay :: Day -> Builder
bDay day' =
  let (y, m, d) = toGregorian day'
  in integerDec y <> charUtf8 '-' <> int2Dec m <> charUtf8 '-' <> int2Dec d

{-| 23-OCT-2013 |-}
bDayReuters :: Day -> Builder
bDayReuters x =
  int2Dec d <> charUtf8 '-' <> bMonth m <> charUtf8 '-' <> integerDec y
  where
    (y, m, d) = toGregorian x

bTime :: TimeOfDay -> Builder
bTime t =
  int2Dec (todHour t) <> charUtf8 ':' <> int2Dec (todMin t) <> charUtf8 ':' <> bPico (todSec t)
-- {-# INLINE bTime #-}

bTime' :: TimeOfDay -> Builder
bTime' t =
  stringUtf8 $ formatTime Data.Time.defaultTimeLocale "%H:%M:%S%Q" t
-- {-# INLINE bTime' #-}

bUTCTime :: UTCTime -> Builder
bUTCTime t =
  bDay (utctDay t) <> charUtf8 ' ' <> bDiffTime (utctDayTime t)

bUTCTime' :: UTCTime -> Builder
bUTCTime' t =
  stringUtf8 $ formatTime Data.Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" t

bDiffTime :: DiffTime -> Builder
bDiffTime t = bTime $ timeToTimeOfDay t

bDiffTime' :: DiffTime -> Builder
bDiffTime' t = bTime' $ timeToTimeOfDay t

-- {-# INLINE bDiffTime #-}
bMonth :: Int -> Builder
bMonth x =
  case x of
    1  -> byteString "JAN"
    2  -> byteString "FEB"
    3  -> byteString "MAR"
    4  -> byteString "APR"
    5  -> byteString "MAY"
    6  -> byteString "JUN"
    7  -> byteString "JUL"
    8  -> byteString "AUG"
    9  -> byteString "SEP"
    10 -> byteString "OCT"
    11 -> byteString "NOV"
    12 -> byteString "DEC"
    _  -> byteString "XYZ"

roundToMilli :: (RealFrac s, Fractional a) => s -> a
roundToMilli n = fromIntegral (round $ n * 1000 :: Integer) / 1000
--{-# INLINE roundToMilli #-}

partMilli :: DiffTime -> Int
partMilli p =
  part
  where
    m = roundToMilli $ realToFrac p
    whole = fromIntegral (floor m :: Integer)
    part = fromIntegral (round $ m * 1000 :: Integer) - whole * 1000

int2Dec :: Int -> Builder
int2Dec i
  | i >= 0 && i < 10 = charUtf8 '0' <> intDec i
  | i >= 0 && i < 100 = intDec i
  | otherwise = error ("show2: the integer i must satisfy 0 <= i < 100: " ++ show i)
--{-# INLINE int2Dec #-}

int3Dec :: Int -> Builder
int3Dec i
  | i >= 0 && i < 10 = charUtf8 '0' <> charUtf8 '0' <> intDec i
  | i >= 0 && i < 100 = charUtf8 '0' <> intDec i
  | i >= 0 && i < 1000 = intDec i
  | otherwise = error ("show2: the integer i must satisfy 0 <= i < 100: " ++ show i)
--{-# INLINE int3Dec #-}

bPico :: Pico -> Builder
bPico p =
  int2Dec whole <> charUtf8 '.' <> int3Dec part
  where
    m = roundToMilli p
    whole = fromIntegral (floor m :: Integer)
    part = fromIntegral (round $ m * 1000 :: Integer) - whole * 1000
--{-# INLINE bPico #-}

{-| 23-10-2013 |-}
pDate :: AC.Parser Day
pDate = do
  y <- decimal
  _ <- AC.char '-'
  m <- decimal
  _ <- AC.char '-'
  d <- decimal
  return $ fromGregorian y m d

{- 20140826 -}
pDate' :: AC.Parser Day
pDate' = do
  _ <- "20"
  y0 <- digit'
  y1 <- digit'
  m0 <- digit'
  m1 <- digit'
  d0 <- digit'
  d1 <- digit'
  return $ fromGregorian (2000 + toEnum (y0 * 10 + y1)) (m0 * 10 + m1) (d0 * 10 + d1)

  where
    digit' = digitToInt <$> digit

{-| 23/10/2013 |-}
pDateIn :: AC.Parser Day
pDateIn = do
  m <- decimal
  _ <- AC.char '/'
  d <- decimal
  _ <- AC.char '/'
  y <- decimal
  return $ fromGregorian y m d
{-# INLINABLE pDateIn #-}

{-| 23-OCT-2013 |-}
pDayReuters :: AC.Parser Day
pDayReuters = do
  d <- decimal <* "-"
  m <- pMonth <* "-"
  y <- decimal
  return $ fromGregorian y m d
{-# INLINE pDayReuters #-}

pTime :: AC.Parser TimeOfDay
pTime =
  TimeOfDay <$>
  decimal <* ":" <*> decimal <* ":" <*> (realToFrac <$> double)
{-# INLINABLE pTime #-}

pDiffTime :: AC.Parser DiffTime
pDiffTime = do
  h <- decimal <* ":"
  m <- decimal <* ":"
  s <- double
  let picos = 10 ^ (12 :: Integer) :: Integer
  return $ secondsToDiffTime (((3600 :: Integer) * h) + ((60 :: Integer) * m)) + picosecondsToDiffTime (floor (fromIntegral picos * s))
--{-# INLINE pDiffTime #-}

pNominalDiffTime :: AC.Parser NominalDiffTime
pNominalDiffTime = do
  x <- double
  return $ fromDouble x

-- | 2013-12-31 12:12:12.123
pUTCTime :: AC.Parser UTCTime
pUTCTime = do
  d <- pDate
  _ <- AC.char ' '
  t <- pTime
  return $ UTCTime d (timeOfDayToTime t)
{-# INLINE pUTCTime #-}

pMonth :: AC.Parser Int
pMonth =
  "JAN" *> return 1 <|>
  "FEB" *> return 2 <|>
  "MAR" *> return 3 <|>
  "APR" *> return 4 <|>
  "MAY" *> return 5 <|>
  "JUN" *> return 6 <|>
  "JUL" *> return 7 <|>
  "AUG" *> return 8 <|>
  "SEP" *> return 9 <|>
  "OCT" *> return 10 <|>
  "NOV" *> return 11 <|>
  "DEC" *> return 12
{-# INLINE pMonth #-}

-- | round down by grain
floorTime :: UTCTime -> NominalDiffTime -> UTCTime
floorTime t grain =
  let ti' = toInteger $ fromEnum grain
      day' = utctDay t
      t' = toInteger $ fromEnum $ utctDayTime t
  in UTCTime day' $ picosecondsToDiffTime (ti' * div t' ti')

-- | round down by grain
ceilingTime :: UTCTime -> NominalDiffTime -> UTCTime
ceilingTime t grain =
  let ti' = toInteger $ fromEnum grain
      day' = utctDay t
      t' = toInteger $ fromEnum $ utctDayTime t
  in UTCTime day' $ picosecondsToDiffTime (ti' * (div t' ti' + 1))

timeSteps :: UTCTime -> UTCTime -> NominalDiffTime -> Integer
timeSteps t t' grain = div (tInt' - tInt) (toInteger $ fromEnum grain)
  where
    tInt = toInteger $ fromEnum $ utctDayTime t
    tInt' = toInteger $ fromEnum $ utctDayTime t'

-- | Time multiplication
(-*-) :: NominalDiffTime -> Double -> NominalDiffTime
(-*-) a b = toEnum $ fromEnum $ fromIntegral (fromEnum a) * b

-- | converters
toSecs :: DiffTime -> Integer
toSecs = floor . (\x -> x / ((10 :: Double) ^ (12 :: Integer))) . fromIntegral . fromEnum

toDouble :: NominalDiffTime -> Double
toDouble t = fromIntegral (fromEnum t) / 10 ^ (12 :: Integer)

toSecs' :: NominalDiffTime -> Double
toSecs' x = fromIntegral (floor (1000 * x)) / 1000

fromDouble :: Double -> NominalDiffTime
fromDouble d = toEnum $ fromEnum (d * 10 ^ (12 :: Integer))

fromDouble' :: Double -> NominalDiffTime
fromDouble' x =
  let d0 = ModifiedJulianDay 0
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime d0 (picosecondsToDiffTime $ floor (x / 1.0e-12))
  in diffUTCTime t1 t0

-- | zero times
zeroDiffTime :: DiffTime
zeroDiffTime = secondsToDiffTime 0

zeroNominalDiffTime :: NominalDiffTime
zeroNominalDiffTime = 0 :: NominalDiffTime

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 0 1 1) zeroDiffTime

zeroTimeLocal :: LocalTime
zeroTimeLocal = LocalTime (toEnum 0) (TimeOfDay 0 0 (realToFrac 0))

-- | general time utilities
{- $stamp -}
stamp :: a -> IO (UTCTime, a)
stamp e = do
  t <- getCurrentTime
  return (t, e)

-- | the exact time format to correct millisecond
fTimeMilli :: DiffTime -> Text
fTimeMilli t = sformat (hms % "." % (left 3 '0' %. int)) (timeToTimeOfDay t) (partMilli t)
