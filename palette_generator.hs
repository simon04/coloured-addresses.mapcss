import Data.Char

--colors = ["00AADD", "DD00AA", "DD0011", "DDAA00", "AADD00"]
-- from http://colorbrewer2.org/
colors = ["A6CEE3", "1F78B4", "B2DF8A", "33A02C", "FB9A99", "E31A1C", "FDBF6F", "FF7F00", "CAB2D6", "6A3D9A", "FFFF99"]
hash i j = mod ((mod i 13) + (mod j 19)) (length colors)

a = ['A'..'Z']++['a'..'z']
b = [[ord i, ord j] | i <- a, j <- a]
c = [([i,j], colors!!(hash (ord i) (ord j))) | i <- a, j <- a]

rule (prefix, color) =
  "area[building]" ++ prefixfun "addr:street" prefix ++ "{" ++ colorfun "color" ++ colorfun "fill-color" ++ "}\n" ++
  "way" ++ prefixfun "name" prefix ++ "{" ++ colorfun "color" ++ "}\n"
  where
    prefixfun k v = "[\"" ++ k ++ "\"^=\"" ++ v ++ "\"]"
    colorfun k = k ++ ": #" ++ color ++ "; "

main = do
  putStr "meta {title: \"Colored Addresses\";}"
  foldr (>>) (putStr "") [putStr $ rule i | i <- c]
