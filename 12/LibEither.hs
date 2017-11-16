module LibEither where

lefts' :: [Either a b] -> [a]
-- [Data.Either]
-- lefts' x = [a | Left a <- x]
--
-- [With foldr]
lefts' = foldr (\ x -> case x of
                         Left  a -> (a:)
                         Right b -> id  ) []


rights' :: [Either a b] -> [b]
rights' = foldr (\ x -> case x of
                         Left  a -> id
                         Right b -> (b:)) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
-- eitherMaybe' f (Right b) = Just (f b)
-- eitherMaybe' _ (Left  _) = Nothing
--
-- [With either' function]
eitherMaybe' f e@(Right b) = Just (either' undefined f e)
eitherMaybe' _   (Left  _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  a) = f a
either' _ g (Right b) = g b
