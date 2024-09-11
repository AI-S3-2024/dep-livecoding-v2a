module Lib where

import Prelude hiding (map, foldr, foldr1, foldl, foldl1, zipWith)

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

data Kleur = R | G | B
  deriving Show

data DrieDingen = Doos String Bool Int
  deriving Show

data TweeDingen a = Tuple a a
  deriving Show

mijnTuple :: TweeDingen Int
mijnTuple = Tuple 4 3

maakTuple :: Int -> TweeDingen Int
maakTuple a = Tuple a a

maakTuple2 :: Int -> Int -> TweeDingen Int
maakTuple2 a b = Tuple a b

data Typenaam a = Geval1 Int | Geval2 a | Geval3
  deriving Show

data Coord = FloatCoord Float Float | IntCoord Int Int
  deriving Show

len :: Coord -> Float
len (FloatCoord x y) = undefined x y
len (IntCoord x y) = undefined x y

data Of a b = Links a | Rechts b
  deriving Show

test :: Of Int String
test = Links 3
--test = Rechts "Drie"

printOf :: Of Int String -> String
printOf (Links a) = show a
printOf (Rechts b) = b

palindroom :: String -> Bool
palindroom [] = True
palindroom [_] = True
palindroom string
  | head string == last string =
      palindroom $ init $ tail string
  | otherwise = False               

if_then_else :: Bool -> a -> a -> a
if_then_else True  if_val _        = if_val
if_then_else False _      else_val = else_val

data MayFail a = Result a | Error String | TotalFail
  deriving Show

divide :: Float -> Float -> MayFail Float
divide _ 0.0 = Error "Nope."
divide a b   = Result $ a / b

handleFail :: MayFail Float -> String
handleFail (Result f) = show f
handleFail (Error s) = s
handleFail TotalFail = "FUBAR"

data Student = Student String (Maybe Double)

polynoom :: (Int, Int) -> Int -> Int
polynoom (a, b) x = a * x ^ b


polynoom' :: Int -> Int -> Int -> Int
polynoom' a b x = polynoom (a, b) x


polynoom_a :: Int -> Int -> Int
polynoom_a = polynoom' 3

polynoom_b :: Int -> Int -> Int
polynoom_b a = polynoom' a 2


keer :: [Int] -> Int
keer = product

--f :: (Float -> Float) -> Float
--f fun = fun 0.7
--
--g :: Float -> (Float -> Float)
--g  = (+) 
--
--h :: (Float -> Float) -> (Float -> Float)
--h fun x = fun x + 4.2



map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


fold :: (a -> a -> a) -> [a] -> a
fold _ [x]    = x
fold f (x:xs) = f x $ fold f xs







