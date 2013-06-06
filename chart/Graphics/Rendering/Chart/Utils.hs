module Graphics.Rendering.Chart.Utils(
    isValidNumber,
    maybeM,
  ) where

isValidNumber :: (RealFloat a) => a -> Bool
isValidNumber v = not (isNaN v) && not (isInfinite v)

maybeM :: (Monad m) => b -> (a -> m b) -> Maybe a -> m b
maybeM v = maybe (return v)


