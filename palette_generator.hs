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

-- from http://colorbrewer2.org/
colors = [
  "A6CEE3", "1F78B4", "B2DF8A", "33A02C",
  "FB9A99", "E31A1C", "FDBF6F", "FF7F00",
  "CAB2D6", "6A3D9A", "FFFF99"]
hash i j = mod ((mod i 13) + (mod j 19)) (length colors)

prefix_color = [([i,j], colors!!(hash (ord i) (ord j))) |
  i <- ['A'..'Z']++"ÄÖÜ",
  j <- ['a'..'z']++"äöüß "]

rule (prefix, color) =
  "area[building]" ++ prefixfun "addr:street" prefix ++ " {" ++ colorfun "color" ++ colorfun "fill-color" ++ "}\n" ++
  "way" ++ prefixfun "name" prefix ++ " {" ++ colorfun "color" ++ "}\n"
  where
    prefixfun k v = "[\"" ++ k ++ "\"^=\"" ++ v ++ "\"]"
    colorfun k = k ++ ": #" ++ color ++ "; "

main = do
  putStr "meta {title: \"Coloured Addresses\"; author: \"simon04\"; link: \"https://github.com/simon04/coloured-addresses.mapcss\"; description: \"Style to ease mapping of addresses by colouring streets and houses\"; }\n"
  foldr ((>>) . putStr . rule) (putStr "") prefix_color
