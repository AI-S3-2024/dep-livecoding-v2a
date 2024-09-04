module Lib where

someFunc :: IO ()
someFunc = putStrLn "Hello"

-- | Calculates the answer...
foo :: Int
foo = 42

formule :: Int -> Int -> Int -> Int
formule a b c = a * b + c

zelfde :: Int -> Int -> Int -> Int
zelfde a b c = formule a b c

zelfde2 :: Int -> Int -> Int -> Int
zelfde2 = formule

somkwadraat :: [Int] -> Int
somkwadraat [] = 0
somkwadraat [g] = g * g
--somkwadraat (g:gs) = let kw = g * g
                         --rest = somkwadraat gs
                     --in kw + rest
somkwadraat (g:gs) = kw + rest
  where kw = g * g
        rest = somkwadraat gs

som :: Int -> Int -> Int
som a b = a + b

plus3 :: Int -> Int
plus3 = som 3

lengte_matig :: [Int] -> Int
lengte_matig [] = 0
lengte_matig [_] = 1
lengte_matig [_,_] = 2
lengte_matig _ = -1

lengte :: [Int] -> Int
lengte [] = 0
lengte (_:xs) = 1 + lengte xs

zeg :: Int -> String
zeg 1 = "EEN"
zeg 2 = "TWEE"
zeg _ = "IETS ANDERS"

median :: [Float] -> Float
median [] = 0.0
median [a] = a
median [a,b] = (a+b)/2
median list = median (init (tail list))

aantal_oplossingen :: Float -> Float -> Float -> Int
aantal_oplossingen a b c
  | delta == 0.0 = 1
  | delta > 0    = 2
  | otherwise    = 0
 where delta = b*b-4*a*c 
