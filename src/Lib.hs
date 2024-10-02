{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.List (uncons)

import Prelude hiding (map, zipWith)
import Data.Char (toUpper, toLower)

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
fold _ []     = error "unsupported"



my_sum :: [Int] -> Int
my_sum = foldr (+) 0

data Point3D = MakePoint String Double Double Double deriving Show --                      naam,  x,     y,     z

-- | Return the naam
-- >>> naam (Point3D "hallo" 1 2 3)
-- "hallo"
naam :: Point3D -> String
naam (MakePoint n _ _ _) = n

get_x :: Point3D -> Double
get_x (MakePoint _ x _ _) = x

-- | Update a datapoint name
with_naam :: String  -- ^ New name
          -> Point3D -- ^ Point to modify
          -> Point3D -- ^ Updated point
with_naam s (MakePoint _ x y z) = MakePoint s x y z

origin :: Point3D
origin = MakePoint "Origin" 0 0 0 -- Bijvoorbeeld

testPoint :: (String, Double, Point3D, String)
testPoint = (
  naam origin,                -- => "Origin"
  get_x origin,                   -- => 0.0
  with_naam "Here" origin,    -- => MakePoint "Here" 0.0 0.0 0.0
  naam (with_naam "Here" origin) -- => "Here"
  )





--som_mod_3 :: [Int] -> String
--som_mod_3 xs = beschrijf (sum xs `mod` 3)
  --where beschrijf 0 = "Deelbaar door drie"
        --beschrijf 1 = "Een teveel"
        --beschrijf 2 = "Een te weinig"

som_mod_3 :: [Int] -> String
som_mod_3 xs = case sum xs `mod` 3 of
  0 -> "Deelbaar door drie"
  1 -> "Een teveel"
  2 -> "Een te weinig"
  _ -> "Dit is onmogelijk"


-- som_mod_3 [3] -> "Deelbaar door drie"
-- som_mod_3 [3,1] -> "Een teveel" (want 4%3=1)
-- som_mod_3 [1,1,1,1,1,1,1,1] -> "Een te weinig" (want 8%3=2)



(+++) :: Int -> Int -> Int
0 +++ 0 = 0
_ +++ _ = -1





data Nul
type Een = ()
type Twee = Bool
data Drie = Rood | Groen | Blauw deriving Show
data Vier = Harten | Schoppen | Ruiten | Klaveren deriving Show
data Vijf = Fire | Water | Earth | Air | Heart deriving Show
data Zes = Up | Down | Charm | Strange | Top | Bottom deriving Show

type EenPlus a = Maybe a   -- Just a of Nothing
type Plus a b = Either a b -- Left a of Right b
type Keer a b = (a, b)     -- (a, b)

datatypes_a :: [EenPlus Vier]
datatypes_a = [Just Harten, Just Schoppen, Just Klaveren, Just Ruiten, Nothing]

datatypes_b :: [Plus Een Drie]
datatypes_b = [Left (), Right Rood, Right Groen, Right Blauw]

datatypes_c :: [Plus Drie Een]
datatypes_c = [Right (), Left Rood, Left Groen, Left Blauw]

datatypes_d :: [Keer Een Vijf]
datatypes_d = [((), Fire), ((), Water), ((), Earth), ((),Air), ((), Heart)]

datatypes_e :: [Keer Drie Twee]
datatypes_e = [(kleur, bool) | kleur <- [Rood, Groen, Blauw], bool <- [True, False]]

datatypes_f :: [Plus Nul Zes]
datatypes_f = [Right Up, Right Down, Right Charm, Right Strange, Right Top, Right Bottom]

datatypes_g :: [Keer Nul Twee]
datatypes_g = []

voorbeeld :: Plus Een Drie -> Keer Twee Twee
voorbeeld (Left ())     = (True, True)
voorbeeld (Right Rood)  = (True, False)
voorbeeld (Right Groen) = (False, False)
voorbeeld (Right Blauw) = (True, False)

heen :: Plus Vijf Twee -> EenPlus (Keer Drie Twee)
heen (Left Fire)   = Just (Rood, True)
heen (Left Water)  = Just (Rood, False)
heen (Left Earth)  = Just (Groen, True)
heen (Left Air)    = Just (Groen, False)
heen (Left Heart)  = Just (Blauw, True)
heen (Right True)  = Just (Blauw, False)
heen (Right False) = Nothing

terug :: EenPlus (Keer Drie Twee) -> Plus Vijf Twee 
terug (Just (Rood, True))   = (Left Fire)
terug (Just (Rood, False))  = (Left Water)
terug (Just (Groen, True))  = (Left Earth)
terug (Just (Groen, False)) = (Left Air)
terug (Just (Blauw, True))  = (Left Heart)
terug (Just (Blauw, False)) = (Right True)
terug Nothing               = (Right False)

functie_a :: Keer Twee Twee -> Zes
functie_a (True, False) = Up
functie_a (False, False) = Down
functie_a (False, True) = Top
functie_a (True, True) = Bottom

functie_b :: Drie -> Plus Een Een
functie_b = undefined
functie_c :: Twee -> Nul
functie_c = undefined
functie_d :: Nul -> Een
functie_d = undefined

data Number = Zero | Next Number

instance Show Number where
  show Zero = ""
  show (Next a) = '|' : show a

one, two, three :: Number
one = Next Zero
two = Next one
three = Next two

plus, times :: Number -> Number -> Number

plus Zero b = b
plus (Next a) b = Next $ plus a b
--plus (Next a) b = plus a $ Next b

times Zero _ = Zero
times (Next Zero) b = b
times (Next a) b = plus b $ times a b


instance Eq Number where
  Zero     == Zero = True
  (Next a) == (Next b) = a == b
  _        == _ = False

instance Ord Number where
  Zero   <= _      = True
  Next a <= Next b = a <= b
  _      <= _      = False


instance Enum Number where
  toEnum 0   = Zero
  toEnum int | int > 0   = Next $ toEnum (int - 1)
             | otherwise = error "Big nope."

  fromEnum Zero = 0
  fromEnum (Next n) = fromEnum n + 1



instance Num Number where

  -- Deze twee hebben we al:
  a + b = plus a b
  a * b = times a b

  -- abs :: Num n => n -> n
  -- Geef de absolute waarde. Die is altijd hetzelfde voor ons type:
  abs = id

  -- signum :: Num n => n -> n
  -- Geeft 0, 1 of -1 terug om aan te geven of een getal positief, negatief, of nul is
  signum Zero = Zero
  signum _    = one
  -- instance Num Number where       -- nog steeds

  -- negate :: Num n => n -> n
  -- Negatieve getallen worden niet ondersteund
  negate _ = undefined

  -- fromInteger :: Num n => Integer -> n
  -- Conversie van een geheel getal (input) naar een waarde van ons type
  -- Integer is een ander type dan Int :-/

  -- We hebben al toEnum :: Int -> Number, en Integer is ook een Enum

  fromInteger = toEnum . fromEnum
  --            ^ Int -> Number
  --                     ^ Integer -> Int

answer :: Number
answer = 42

testVal :: Number -> String -- Dit kan alleen als Number een instance van Eq en Num is
testVal 0 = "Nil"
testVal 1 = "Uno"
testVal 2 = "Muy"
testVal _ = "No bueno"


newtype Additive = Add Number

instance Semigroup Additive where
  (Add a) <> (Add b) = Add $ plus a b                     -- (<>) :: Number -> Number -> Number

instance Monoid Additive where
  mempty = Add Zero                   -- mempty :: Number

newtype Multiplicative = Mul Number

instance Semigroup Multiplicative where
  (Mul a) <> (Mul b) = Mul $ times a b                     -- (<>) :: Number -> Number -> Number

instance Monoid Multiplicative where
  mempty = Mul one                   -- mempty :: Number







infixr 5 :::
data Stream a = a ::: Stream a

headS :: Stream a -> a
headS (h ::: _) = h

tailS :: Stream a -> Stream a
tailS (_ ::: t) = t

repeatS :: a -> Stream a
repeatS a = a ::: repeatS a

-- repeatS 1 => 1 ::: 1 ::: 1 ::: 1 ::: 1 ::: ...

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (a ::: as) = a : takeS (pred n) as

prepend :: [a] -> Stream a -> Stream a
prepend [] stream = stream
prepend (a:as) stream = a ::: prepend as stream



cycleS :: [a] -> Stream a
cycleS as = prepend as $ cycleS as

--cycleS (a:as) = a ::: cycleS (as ++ [a])

mapS :: (a -> b) -> Stream a -> Stream b
mapS f (a ::: as) = f a ::: mapS f as


zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS op (a ::: as) (b ::: bs) = a `op` b ::: zipWithS op as bs

fibs :: Stream Int
fibs = 0 ::: 1 ::: zipWithS (+) fibs (tailS fibs)

iterateS :: (a -> a) -> a -> Stream a
iterateS f current = current ::: iterateS f (f current)

naturals :: Stream Int
naturals = iterateS succ 0

-- takeS 10 naturals = [0,1,2,3,4,5,6,7,8,9]

scanS :: (b -> a -> b) -> b -> Stream a -> Stream b
scanS op b (a ::: as) = b ::: scanS op (b `op` a) as

-- takeS 10 $ scanS (+) 0 $ tailS naturals
--    = [0,1,3,6,10,15,21,28,36,45]

instance Functor Stream where
  --fmap :: (a -> b) -> Stream a -> Stream b
  fmap = mapS

capitalise :: Stream Char -> Stream Char
capitalise cs = fmap toUpper cs

double :: Num a => Stream a -> Stream a
double as = (*2) <$> as


mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just a) = Just $ f a 

contained_in :: (Eq t, Num a) => t -> [t] -> Maybe a
contained_in _ []                                  = Nothing
contained_in target (first:rest) | target == first = Just 0
                                 | otherwise       = maybe_plus_een $ contained_in target rest
  where maybe_plus_een (Just x) = Just $ x + 1
        maybe_plus_een Nothing  = Nothing

contained' :: (Eq t, Num a) => t -> [t] -> Maybe a
contained' _ [] = Nothing
contained' target (first:rest) | target == first = Just 0
                               | otherwise = (+1) <$> contained' target rest


instance Applicative Stream where
  pure = repeatS
  --liftA2 = zipWithS
  (f:::fs) <*> (a:::as) = f a ::: (fs <*> as)










data Indexed a = Indexed { index :: ((Int, Int) -> a) }

{-
instance Show (Indexed Char) where
  show i = unlines [ [ (index i) (x,y)
                     | x <- [0..10]
                     ]
                   | y <- [0..10]
                   ]
-}

figO :: Indexed Char
figO = Indexed $ \(x,y) ->
  if (x == 0 || x == 2 || y == 0 || y == 2)
       && (x < 3) && (y < 3)
    then 'I'
    else '.'

figX :: Indexed Char
figX = Indexed $ \(x,y) -> case (x,y) of
  (0, 0) -> 'A'
  (2, 0) -> 'B'
  (1, 1) -> 'C'
  (0, 2) -> 'D'
  (2, 2) -> 'E'
  _ -> ' '

figSlash :: Indexed Char
figSlash = Indexed $ \(x,y) -> if x == y then '\\' else '/'


instance Functor Indexed where
  fmap f (Indexed i) = Indexed $ f . i


mirror :: Char -> Char
mirror c | c == '\\' = '/'
         | c == '/'  = '\\'
         | otherwise = c

applyRows :: [Int] -> (a -> a) -> Indexed (a -> a)
applyRows rows f = Indexed $ \(_, y) -> if y `elem` rows then f else id

applyCols :: [Int] -> (a -> a) -> Indexed (a -> a)
applyCols cols f = Indexed $ \(x, _) -> if x `elem` cols then f else id  

instance Applicative Indexed where
  pure a = Indexed $ \_ -> a
  Indexed fs <*> Indexed as = Indexed $ \(x,y) -> (fs (x,y)) (as (x,y))


testI1, testI2, testI3 :: Indexed Char
testI1 = applyCols [0, 2] mirror <*> figSlash
testI2 = applyCols [0] toUpper <*> (applyRows [0,2] toLower <*> figO)
testI3 = min <$> figX <*> figO


data Boring a = Packed { unpack :: a } deriving Show

instance Functor Boring where
  --fmap :: (a -> b) -> Boring a -> Boring b
  fmap f bv = Packed . f . unpack $ bv

instance Applicative Boring where
  --pure :: a -> Boring a
  pure = Packed
  -- (<*>) :: Boring (a -> b) -> Boring a -> Boring b
  (Packed f) <*> (Packed v) = Packed $ f v

boring_inc, boring_dec :: Int -> Boring Int
boring_inc a = Packed $ a + 1  -- met de constructor
boring_dec a = pure $ a - 1    -- of generic `pure`

instance Monad Boring where
  -- (>>=) :: Boring a -> (a -> Boring b) -> Boring b
  (Packed a) >>= f = f a


get_reverse :: IO String -- o.b.v. getLine en reverse
get_reverse = reverse <$> getLine

print_reverse :: IO ()   -- o.b.v. get_reverse en putStrLn
print_reverse = get_reverse >>= putStrLn






-- | Input a number and then read that many chars
input_n :: IO String
input_n = let read_chars num = take num <$> getLine
          in readLn >>= read_chars

hallo :: IO ()
hallo = do putStrLn "Hoe heet jij?"
           name <- getLine
           putStrLn $ "Hallo, " <> name ++ "!"

avg :: IO ()
avg = do content <- readFile "input"
         let nums = map read $ lines content :: [Double]
         print nums
         print $ sum nums / fromIntegral (length nums)


-- Voer uit met runState
handleInput :: State [Int] Int
handleInput = do ints <- get
                 put $ tail ints
                 return $ head ints

-- Alleen resultaat van belang -> evalState
sumInput :: State [Int] Int
sumInput = do input <- uncons <$> get
              case input of
                Just (h, t) -> do put t
                                  val <- sumInput
                                  return $ val + h
                                  --(+h) <$> sumInput
                Nothing -> return 0

-- Alleen uiteindelijke state van belang -> execState
produce :: State [Int] ()
produce = do s <- get
             if s == []
               then put [0]
               else modify $ (head s + 1 :)

produceN :: Int -> State [Int] ()
produceN 0 = return ()
produceN n = produce >> produceN (n-1)


new_stack :: State [Int] ()      -- [a, b, ...]   ~>   [1, a, b, ...]
new_stack = modify (1:)

--new_stack = do s <- get
               --put $ (1:) s

reverse_stacks :: State [Int] () -- [a, b, c]     ~>   [c, b, a]
reverse_stacks = modify reverse

dup :: State [Int] ()            -- [a, b]        ~>   [a, b, a, b]
dup = modify $ \x -> x <> x

--add_1 :: State [Int] ()          -- [a, b, ...]   ~>   [a + 1, b, ...]

type Parser = (StateT String Maybe) -- Parser a   ~>   State String (Maybe a)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

pOptional :: Parser a -> Parser (Maybe a)
pOptional p = Just <$> p <|> return Nothing 

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

readFromChar :: Char -> Int
readFromChar = read . (: [])

pDigit :: Parser Int
pDigit = readFromChar <$> pCharSet "0123456789"

pDigitTimesTen :: Parser Int
pDigitTimesTen = (*10) <$> pDigit

pDoubleDigits :: Parser Int
pDoubleDigits = (+) <$> pDigitTimesTen <*> pDigit -- A la zipWith

pDigitTimesHundred :: Parser Int
pDigitTimesHundred = (* 100) <$> pDigit

pTripleDigits :: Parser Int
pTripleDigits = (+) <$> pDigitTimesHundred <*> pDoubleDigits


pNumber :: Parser Int -- Let op de volgorde!
pNumber = pTripleDigits <|> pDoubleDigits <|> pDigit


