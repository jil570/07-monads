module MaybePractice where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Test.HUnit
import qualified Text.Read as Text

-- 1

data Weather = Weather
  { dayNumber :: Int,
    maxTemp :: Int,
    minTemp :: Int
  }
  deriving (Eq, Show)

parseWeatherTest1, parseWeatherTest2, parseWeatherTest3 :: Test
parseWeatherTest1 = parseWeather m ~?= Just Weather {dayNumber = 2, maxTemp = 78, minTemp = 62}
  where
    m = Map.fromList [("day", "2"), ("maxTemp", "78"), ("minTemp", "62")]
parseWeatherTest2 = parseWeather m ~?= Nothing
  where
    m = Map.fromList [("day", "2")]
parseWeatherTest3 = parseWeather m ~?= Nothing
  where
    m = Map.fromList [("day", "two"), ("maxTemp", "78"), ("minTemp", "62")]

parseWeather :: Map String String -> Maybe Weather
parseWeather x = do
  daymb <- Map.lookup "day" x
  maxmb <- Map.lookup "maxTemp" x
  minmb <- Map.lookup "minTemp" x
  dayNumber <- Text.readMaybe daymb
  maxTemp <- Text.readMaybe maxmb
  minTemp <- Text.readMaybe minmb
  return (Weather dayNumber maxTemp minTemp)

-- 2

-- | Left-biased choice on maybes
--
-- >>> firstJust (Just 1) (Just 2)
-- Just 1
-- >>> firstJust Nothing (Just 2)
-- Just 2
-- >>> firstJust (Just 1) Nothing
-- Just 1
-- >>> firstJust Nothing Nothing
-- Nothing
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust = undefined

-- | Right-biased choice on maybes
--
-- >>> secondJust (Just 1) (Just 2)
-- Just 2
-- >>> secondJust Nothing (Just 2)
-- Just 2
-- >>> secondJust (Just 1) Nothing
-- Just 1
-- >>> secondJust Nothing Nothing
-- Nothing
secondJust :: Maybe a -> Maybe a -> Maybe a
secondJust = undefined

-- | Ensure that both Maybes are 'Just' and retain the first one
--
-- >>> sequenceFirst (Just 1) (Just 'a')
-- Just 1
-- >>> sequenceFirst Nothing (Just 'a')
-- Nothing
-- >>> sequenceFirst (Just 1) Nothing
-- Nothing
-- >>> sequenceFirst Nothing Nothing
-- Nothing
sequenceFirst :: Maybe a -> Maybe b -> Maybe a
sequenceFirst = undefined

-- | Ensure that both Maybes are 'Just' and retain the second one
--
-- >>> sequenceSecond (Just 1) (Just 'a')
-- Just 1
-- >>> sequenceSecond Nothing (Just 'a')
-- Nothing
-- >>> sequenceSecond (Just 1) Nothing
-- Nothing
-- >>> sequenceSecond Nothing Nothing
-- Nothing
sequenceSecond :: Maybe a -> Maybe b -> Maybe b
sequenceSecond = undefined
