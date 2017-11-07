module LibMaybe where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d _ Nothing   = d
mayybee d f (Just x)  = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe d m = mayybee d id m

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x:xs)  = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing   = []
maybeToList (Just v)  = [v]

catMaybes :: [Maybe a] -> [a]
catMaybes ((Just x):xs) = x : (catMaybes xs)
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes _             = []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | length (filter isNothing xs) > 0  = Nothing
  | otherwise                         = Just (catMaybes xs)
