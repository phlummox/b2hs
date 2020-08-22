{-# OPTIONS_GHC -fforce-recomp #-}

module CompileTime
  (
    embedTime
  , embedISOTime
  )
  where

import            Data.Time (formatTime, defaultTimeLocale, getZonedTime,
                             ZonedTime)
import            Language.Haskell.TH

-- | e.g.:
--
-- @
-- $(embedTime show)
-- @
embedTime :: (ZonedTime -> String) -> Q Exp
embedTime f = do
    tm <- runIO (f <$> getZonedTime)
    return $! LitE (StringL tm)

-- | embed ISO format time, a la "date -Is"
--
-- e.g.
-- >>> :set -XTemplateHaskell
-- >>> $(embedISOTime)
-- "20..."
--
-- (e.g. "2020-03-30T14:01:30+0800")
embedISOTime :: Q Exp
embedISOTime = embedTime (formatTime defaultTimeLocale "%FT%T%z")

