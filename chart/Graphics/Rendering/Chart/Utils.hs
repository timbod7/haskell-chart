-- | Non chart specific utility functions.
module Graphics.Rendering.Chart.Utils(
    isValidNumber,
    maybeM,
  ) where

-- | Checks if the given value is and actual numeric value and not
--   a concept like NaN or infinity.
isValidNumber :: (RealFloat a) => a -> Bool
isValidNumber v = not (isNaN v) && not (isInfinite v)

-- | Version of 'Prelude.maybe' that returns a monadic value.
maybeM :: (Monad m) => b -> (a -> m b) -> Maybe a -> m b
maybeM v = maybe (return v)
