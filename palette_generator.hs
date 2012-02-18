{-
  Copyright (C) 2012  Simon Legner
  https://github.com/simon04/coloured-addresses.mapcss

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
