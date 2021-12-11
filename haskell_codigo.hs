c [] = []
c (p:xs) = c a ++ [p] ++ c b
    where a  = filter (<p) xs
          b = filter (>=p) xs