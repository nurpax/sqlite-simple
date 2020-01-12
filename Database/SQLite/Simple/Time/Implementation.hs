------------------------------------------------------------------------------
-- |
-- Module:      Database.SQLite.Simple.Time.Implementation
-- Copyright:   (c) 2012 Leon P Smith
--              (c) 2012-2014 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
--
-- Adapted from Leon P Smith's code for SQLite.
--
-- See <http://sqlite.org/lang_datefunc.html> for date formats used in SQLite.
------------------------------------------------------------------------------

module Database.SQLite.Simple.Time.Implementation (
    parseUTCTime
  , parseDay
  , utcTimeToBuilder
  , dayToBuilder
  , timeOfDayToBuilder
  , timeZoneToBuilder
  ) where
import           Blaze.ByteString.Builder (Builder)
import           Blaze.ByteString.Builder.Char8 (fromChar)
import           Blaze.Text.Int (integral)
import           Control.Applicative
import           Control.Monad (when)
import qualified Data.Attoparsec.Text as A
import           Data.Bits ((.&.))
import           Data.ByteString.Internal (w2c)
import           Data.Char (isDigit, ord)
import           Data.Fixed (Pico)
import qualified Data.Text as T
import           Data.Time hiding (getTimeZone, getZonedTime)
import           Prelude hiding (take, (++))
import           Unsafe.Coerce

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

parseUTCTime   :: T.Text -> Either String UTCTime
parseUTCTime   = A.parseOnly (getUTCTime <* A.endOfInput)

parseDay :: T.Text -> Either String Day
parseDay = A.parseOnly (getDay <* A.endOfInput)

getDay :: A.Parser Day
getDay = do
    yearStr <- A.takeWhile isDigit
    when (T.length yearStr < 4) (fail "year must consist of at least 4 digits")

    let !year = toNum yearStr
    _       <- A.char '-'
    month   <- digits "month"
    _       <- A.char '-'
    day     <- digits "day"

    case fromGregorianValid year month day of
      Nothing -> fail "invalid date"
      Just x  -> return $! x

decimal :: Fractional a => T.Text -> a
decimal str = toNum str / 10^(T.length str)
{-# INLINE decimal #-}

getTimeOfDay :: A.Parser TimeOfDay
getTimeOfDay = do
    hour   <- digits "hours"
    _      <- A.char ':'
    minute <- digits "minutes"
    -- Allow omission of seconds.  If seconds is omitted, don't try to
    -- parse the sub-second part.
    (sec,subsec)
           <- ((,) <$> (A.char ':' *> digits "seconds") <*> fract) <|> pure (0,0)

    let !picos' = sec + subsec

    case makeTimeOfDayValid hour minute picos' of
      Nothing -> fail "invalid time of day"
      Just x  -> return $! x

    where
      fract =
        (A.char '.' *> (decimal <$> A.takeWhile1 isDigit)) <|> pure 0

getTimeZone :: A.Parser TimeZone
getTimeZone = do
    sign  <- A.satisfy (\c -> c == '+' || c == '-')
    hours <- digits "timezone"
    mins  <- (A.char ':' *> digits "timezone minutes") <|> pure 0
    let !absset = 60 * hours + mins
        !offset = if sign == '+' then absset else -absset
    return $! minutesToTimeZone offset

getUTCTime :: A.Parser UTCTime
getUTCTime = do
    day  <- getDay
    _    <- A.char ' ' <|> A.char 'T'
    time <- getTimeOfDay
    -- SQLite doesn't require a timezone postfix.  So make that
    -- optional and default to +0.  'Z' means UTC (zulu time).
    zone <- getTimeZone <|> (A.char 'Z' *> pure utc) <|> (pure utc)
    let (!dayDelta,!time') = localToUTCTimeOfDay zone time
    let !day' = addDays dayDelta day
    let !time'' = timeOfDayToTime time'
    return (UTCTime day' time'')

toNum :: Num n => T.Text -> n
toNum = T.foldl' (\a c -> 10*a + digit c) 0
{-# INLINE toNum #-}

digit :: Num n => Char -> n
digit c = fromIntegral (ord c .&. 0x0f)
{-# INLINE digit #-}

digits :: Num n => String -> A.Parser n
digits msg = do
  x <- A.anyChar
  y <- A.anyChar
  if isDigit x && isDigit y
  then return $! (10 * digit x + digit y)
  else fail (msg ++ " is not 2 digits")
{-# INLINE digits #-}

dayToBuilder :: Day -> Builder
dayToBuilder (toGregorian -> (y,m,d)) = do
    pad4 y ++ fromChar '-' ++ pad2 m ++ fromChar '-' ++ pad2 d

timeOfDayToBuilder :: TimeOfDay -> Builder
timeOfDayToBuilder (TimeOfDay h m s) = do
    pad2 h ++ fromChar ':' ++ pad2 m ++ fromChar ':' ++ showSeconds s

timeZoneToBuilder :: TimeZone -> Builder
timeZoneToBuilder tz
    | m == 0     =  sign h ++ pad2 (abs h)
    | otherwise  =  sign h ++ pad2 (abs h) ++ fromChar ':' ++ pad2 (abs m)
  where
    (h,m) = timeZoneMinutes tz `quotRem` 60
    sign h | h >= 0    = fromChar '+'
           | otherwise = fromChar '-'

-- | Output YYYY-MM-DD HH:MM:SS with an optional .SSS fraction part.
-- Explicit timezone attribute is not appended as per SQLite3's
-- datetime conventions.
utcTimeToBuilder :: UTCTime -> Builder
utcTimeToBuilder (UTCTime day time) =
    dayToBuilder day ++ fromChar ' ' ++ timeOfDayToBuilder (timeToTimeOfDay time)

showSeconds :: Pico -> Builder
showSeconds xyz
    | yz == 0   = pad2 x
    | z  == 0   = pad2 x ++ fromChar '.' ++  showD6 y
    | otherwise = pad2 x ++ fromChar '.' ++  pad6   y ++ showD6 z
  where
    -- A kludge to work around the fact that Data.Fixed isn't very fast and
    -- doesn't give me access to the MkFixed constructor.
    (x_,yz) = (unsafeCoerce xyz :: Integer)     `quotRem` 1000000000000
    x = fromIntegral x_ :: Int
    (fromIntegral -> y, fromIntegral -> z) = yz `quotRem` 1000000

pad6 :: Int -> Builder
pad6 xy = let (x,y) = xy `quotRem` 1000
           in pad3 x ++ pad3 y

showD6 :: Int -> Builder
showD6 xy = case xy `quotRem` 1000 of
              (x,0) -> showD3 x
              (x,y) -> pad3 x ++ showD3 y

pad3 :: Int -> Builder
pad3 abc = let (ab,c) = abc `quotRem` 10
               (a,b)  = ab  `quotRem` 10
            in p a ++ p b ++ p c

showD3 :: Int -> Builder
showD3 abc = case abc `quotRem` 100 of
              (a, 0) -> p a
              (a,bc) -> case bc `quotRem` 10 of
                          (b,0) -> p a ++ p b
                          (b,c) -> p a ++ p b ++ p c

-- | p assumes its input is in the range [0..9]
p :: Integral n => n -> Builder
p n = fromChar (w2c (fromIntegral (n + 48)))
{-# INLINE p #-}

-- | pad2 assumes its input is in the range [0..99]
pad2 :: Integral n => n -> Builder
pad2 n = let (a,b) = n `quotRem` 10 in p a ++ p b
{-# INLINE pad2 #-}

-- | pad4 assumes its input is positive
pad4 :: (Integral n, Show n) => n -> Builder
pad4 abcd | abcd >= 10000 = integral abcd
          | otherwise     = p a ++ p b ++ p c ++ p d
  where (ab,cd) = abcd `quotRem` 100
        (a,b)   = ab   `quotRem` 10
        (c,d)   = cd   `quotRem` 10
{-# INLINE pad4 #-}
