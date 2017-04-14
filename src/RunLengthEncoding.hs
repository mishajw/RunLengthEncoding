module RunLengthEncoding (encode, decode) where

encode :: Ord a => [a] -> [(Int, a)]
encode (a : as) =
    case encode as of
      as' @ ((i, a') : as'') ->
        if a == a'
        then (i + 1, a') : as''
        else (1, a) : as'
      [] -> [(1, a)]
encode [] = []

decode :: [(Int, a)] -> [a]
decode ((i, a) : as) =
  map (\_ -> a) [1..i] ++ decode as
decode [] = []

