



applyOptional2 :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
applyOptional2 _ Empty _ = Empty
applyOptional2 _ _ Empty = Empty
applyOptional2 f (FUll a) (Full b) = Full(f a b)


