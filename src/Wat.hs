{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Wat where
  import Data.List
  import Data.Char
  import Data.Time
  import Data.Tuple
  import Data.Maybe
  import Control.Applicative
  import Data.Semigroup
  import Data.Monoid hiding ((<>))
  import Test.QuickCheck
  import Test.QuickCheck.Checkers
  import qualified Test.QuickCheck.Classes
  import Control.Exception
  import Control.Monad (join)

  data DayOfWeek =
    Mon | Tue | Wen | Thu | Fri | Sat | Sun
    deriving (Show)
  instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wen Wen = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False
  instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

  class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer

  newtype Age =
    Age Integer deriving (Eq, Show)
  instance Numberish Age where
    fromNumber = Age
    toNumber (Age n) = n

  newtype Year =
    Year Integer deriving (Eq, Show)
  instance Numberish Year where
    fromNumber = Year
    toNumber (Year n) = n

  sumN :: Numberish a => a -> a -> a
  sumN a a' =
    fromNumber summed where
      inta = toNumber a
      inta' = toNumber a'
      summed = inta + inta'

  data Mood = Blah | Woot deriving (Show, Eq)
  settleDown x =
    if x == Woot
       then Blah
       else x

  type Sub = String
  type Verb = String
  type Obj = String
  data Sentence =
    Sentence Sub Verb Obj deriving (Eq, Show)
  s1 = Sentence "dogs" "drool"
  s2 = Sentence "julle" "loves" "dogs"

  data Rocks =
    Rocks String deriving (Eq, Show)
  data Yeah =
    Yeah Bool deriving (Eq, Show)
  data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

  equalityForall :: Papu -> Papu -> Bool
  equalityForall p p' = p == p'

--  comparePapus :: Papu -> Papu -> Bool
--  comparePapus p p' = p > p'

  f :: RealFrac a => a
  f = 1.0

  freud :: Int -> Int
  freud x = x

  myX :: Num a => a
  myX = 1
  sigmund :: Integral a => a -> a
  sigmund x = myX

  jung :: [Int] -> Int
  jung  = minimum

  mySort :: String -> String
  mySort = sort

  signifier :: String -> Char
  signifier xs = head (mySort xs)

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith f int a =
    fromInteger int + f a

  bindExp :: Integer -> String
  bindExp x = let y = 5 in
                  let z = y + x in "the integer was: "
                      ++ show x ++ " and y was: "
                      ++ show y ++ " and z was: " ++ show z

  numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 =1

  pal xs
    | xs == reverse xs = True
    | otherwise  = False

  foldBool3 :: a -> a -> Bool -> a
  foldBool3 x y b =
    if b then x else y

  foldBool4 x y b
    | b = x
    | not b = y

  foldBool5 x y True =
    x
  foldBool5 x y False =
    y

  roundTrip :: (Show a, Read b) => a -> b
  roundTrip a = read (show a)

  main = do
    print (roundTrip 4 :: Int)
    print 4

  ff True = 0
  ff False = error "haha"

  myWords :: String -> [String]
  myWords str = myW str []
    where myW [] acc = reverse acc
          myW s acc =
            let word = takeWhile (/=' ') s
                wordl = length word
                (_, rest) = splitAt wordl s
                rest' = dropWhile (==' ') rest
             in myW rest'  (word:acc)

  myZip :: [a] -> [b] -> [(a,b)]
  myZip [] _ = []
  myZip _ [] = []
  myZip (a:as) (b:bs) = (a,b): myZip as bs

  myZipWith :: (a->b->c) -> [a] -> [b] -> [c]
  myZipWith f [] _ = []
  myZipWith f _ [] = []
  myZipWith f (a:as) (b:bs) =
    f a b : myZipWith f as bs

  myZip1 :: [a] -> [b] -> [(a,b)]
  myZip1 = myZipWith (,)

  myAnd :: [Bool] -> Bool
  myAnd = foldr (&&) True
  --myAnd (x:xs) =  x && myAnd xs

  myAny :: (a->Bool) -> [a] -> Bool
  myAny f = foldr ((||).f) False
  --myAny f [] = False
  --myAny f (x:xs) = f x || myAny f xs

  myOr :: [Bool] -> Bool
  myOr = myAny id

  myElem :: Eq a => a -> [a] -> Bool
  myElem e = any (e==)

  squish :: [[a]] -> [a]
  squish [] = []
  squish (as:ass) = as ++ squish ass

  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap f = foldr ((++).f) []

  squishAgain :: [[a]] -> [a]
  squishAgain = squishMap id

  myMaximumBy :: (a->a->Ordering) -> [a] -> a
  myMaximumBy f [] = undefined
  myMaximumBy f (x:xs) = myMaximumBy' f x xs where
    myMaximumBy' f m [] = m
    myMaximumBy' f m (x:xs) =
      case f m x of
        LT -> myMaximumBy' f x xs
        _ -> myMaximumBy' f m xs

  myMaximum :: (Ord a) => [a] -> a
  myMaximum = myMaximumBy compare

  myMinimumBy :: (a->a->Ordering) -> [a] -> a
  myMinimumBy f = myMaximumBy $ flip f

  myMinimum :: (Ord a) => [a] -> a
  myMinimum = myMinimumBy compare

  data DatabaseItem =
    DbString String |
    DbNumber Integer |
      DbDate UTCTime deriving (Eq, Ord, Show)

  theDatabase :: [DatabaseItem]
  theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)) ]

  filterDbDate :: [DatabaseItem] -> [UTCTime]
  filterDbDate = foldr getDate [] where
    getDate (DbDate d) acc = d:acc
    getDate _ acc = acc

  filterDbNumber :: [DatabaseItem] -> [Integer]
  filterDbNumber = foldr getNumber [] where
    getNumber (DbNumber x) acc = x:acc
    getNumber _ acc = acc

  mostRecent :: [DatabaseItem] -> UTCTime
  mostRecent = maximum.filterDbDate
  -- OR
  m x = maximum $ filterDbDate x

  sumDb :: [DatabaseItem] -> Integer
  sumDb = foldr summy 0 where
    summy (DbNumber x) acc = x + acc
    summy _ acc = acc

  avgDb :: [DatabaseItem] -> Double
  avgDb db = let all = fromIntegral $ sumDb db
                 l = fromIntegral $ length db
              in all/l

  myMap :: (a -> b) -> [a] -> [b]
  myMap f = foldr ((:).f) []

  newtype Goats = Goats Int deriving Show

  class TooMany a where
    tooMany :: a -> Bool
  instance TooMany Int where
    tooMany n = n>42
  instance TooMany Goats where
    tooMany (Goats n) = n>43
  instance TooMany (Int, String) where
    tooMany (int, _) = int>43
  instance TooMany (Int, Int) where
    tooMany (a,b) = tooMany $ a+b
  instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (a,b) = False


  data BinaryTree a =
    Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

  insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
  insert' b Leaf = Node Leaf b Leaf
  insert' b (Node left a right)
    | b > a = Node left a (insert' b right)
    | b < a = Node (insert' b left) a right
    | b == a = Node left a right

  mapTree :: (a->b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

  preOrder :: BinaryTree a -> [a]
  preOrder Leaf = []
  preOrder (Node left a right) =
    [a] ++ preOrder left ++ preOrder right

  inOrder :: BinaryTree a -> [a]
  inOrder Leaf = []
  inOrder (Node left a right) =
    inOrder left ++ [a] ++ inOrder right

  postOrder :: BinaryTree a -> [a]
  postOrder Leaf = []
  postOrder (Node left a right) =
    postOrder left ++ postOrder right ++ [a]

  foldTree :: (a->b->b) -> b -> BinaryTree a -> b
  foldTree _ acc Leaf = acc
  foldTree f acc (Node left a right) =
    let
      l = foldTree f acc left
      r = foldTree f l right
    in
      f a r

  vigenere :: String -> String -> String
  vigenere str key =
    let
      repeat = div (length str) (length key) + 1
      vigenere' "" key = ""
      vigenere' (' ':str) key = ' ' : vigenere' str key
      vigenere' (c:str) (k:key) =
        (chr $ mod (ord c + ord k - 65 - 65) 26 + 65) : vigenere' str key
    in
      vigenere' str (concat $ replicate repeat key)

  isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
  isSubsequenceOf a b = and [x `elem` b | x <- a]

  capitalizeWord :: String -> String
  capitalizeWord str@(x:xs)
    | ord x >= 65 && ord x <= 90 = str
    | otherwise = chr (ord x - 32) : xs

  type Digit = Char
  type Presses = Int
  data DaPhone = DaPhone [(Digit, String)]

  dataphone :: DaPhone
  dataphone = DaPhone [('2',"abc2"),('3',"def3"),('4',"ghi4"),('5',"jkl5"),('6',"mno6"),('7',"pqrs7"),('8',"tuv8"),('9',"wxyz9"),('0'," 0")]

  convo :: [String]
  convo = ["Wanna play 20 questions", "Ya", "U 1st haha", "Lol ok. Have u ever tasted alcohol lol", "Lol ya", "Wow ur cool haha. Ur turn", "Ok. Do u think I am pretty Lol", "Lol ya", "Haha thanks just making sure rofl ur turn"]

  reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
  reverseTaps phone char
    | isUpper char = [('*', 1), lookkey phone (toLower char)]
    | otherwise = [lookkey phone char]

  lookkey :: DaPhone -> Char -> (Digit, Presses)
  lookkey (DaPhone ((digit,str):tl)) char =
    case elemIndex char str of
      Nothing -> lookkey (DaPhone tl) char
      Just idx -> (digit, idx+1)

  cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
  cellPhonesDead phone =
    foldMap (reverseTaps phone)

  fingerTaps :: [(Digit, Presses)] -> Presses
  fingerTaps =
    foldr (\(_,press) acc -> acc + press) 0

  mostPopularLetter :: String -> Char
  mostPopularLetter str =
    let pairs =
          foldr (\c acc ->
            case lookup c acc of
              Nothing -> (c, 1) : acc
              Just rep -> insert (c,rep+1) $ delete (c,rep) acc) [] str
    in
      fst $ maximumBy (\(_,rep1) (_,rep2) -> compare rep1 rep2) pairs


  -- Hutton's Razor
  data Expr
    = Lit Integer
    | Add Expr Expr

  eval :: Expr -> Integer
  eval (Lit int) = int
  eval (Add exp1 exp2) = eval exp1 + eval exp2

  printExpr :: Expr -> String
  printExpr (Lit int) = show int
  printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExpr exp2


  notThe :: String -> Maybe String
  notThe "the" = Nothing
  notThe s = Just s

  replaceThe :: String -> String
  replaceThe str =
    unwords $ (fromMaybe "a" . notThe) <$> words str
--  unwords $ fmap (fromMaybe "a" . notThe) $ words str

  countTheBeforeVowel :: String -> Integer
  countTheBeforeVowel str =
    let
      count' acc [] = acc
      count' acc ("the" : ('a':_) : rest) = count' (acc+1) rest
      count' acc ("the" : ('e':_) : rest) = count' (acc+1) rest
      count' acc ("the" : ('i':_) : rest) = count' (acc+1) rest
      count' acc ("the" : ('o':_) : rest) = count' (acc+1) rest
      count' acc ("the" : ('u':_) : rest) = count' (acc+1) rest
      count' acc (_:t) = count' acc t
    in
      count' 0 $ words str

  countVowels :: String -> Integer
  countVowels str =
    let
      countV' acc [] = acc
      countV' acc ('a':rest) = countV' (acc+1) rest
      countV' acc ('e':rest) = countV' (acc+1) rest
      countV' acc ('i':rest) = countV' (acc+1) rest
      countV' acc ('o':rest) = countV' (acc+1) rest
      countV' acc ('u':rest) = countV' (acc+1) rest
      countV' acc (_:t) = countV' acc t
    in
      countV' 0 str

  newtype Word' =
    Word' String deriving (Eq, Show)

  vowels = "aeiou"

  mkWord :: String -> Maybe Word'
  mkWord str =
    let
      mk' tup [] = tup
      mk' tup (' ':rest) = mk' tup rest
      mk' (vo,cn) ('a':rest) = mk' (vo+1,cn) rest
      mk' (vo,cn) ('e':rest) = mk' (vo+1,cn) rest
      mk' (vo,cn) ('i':rest) = mk' (vo+1,cn) rest
      mk' (vo,cn) ('o':rest) = mk' (vo+1,cn) rest
      mk' (vo,cn) ('u':rest) = mk' (vo+1,cn) rest
      mk' (vo,cn) (_:rest) = mk' (vo,cn+1) rest
      (v,c) = mk' (0,0) str
    in
      if v < c
         then
          Just $ Word' str
         else
          Nothing

  data Nat =
    Zero | Succ Nat deriving (Eq, Show)

  natToInteger :: Nat -> Integer
  natToInteger Zero = 0 :: Integer
  natToInteger (Succ nat) = 1 + natToInteger nat

  integerToNat :: Integer -> Maybe Nat
  integerToNat int
    | int < 0 = Nothing
    | int >= 0 = Just $ itn int
    where
      itn 0 = Zero
      itn i = Succ (itn $ i-1)

  isJust1 :: Maybe a -> Bool
  isJust1 (Just _) = True
  isJust1 Nothing = False

  isNothing1 :: Maybe a -> Bool
  isNothing1 = not.isJust1

  maybee :: b -> (a->b) -> Maybe a -> b
  maybee b f Nothing = b
  maybee b f (Just a) = f a

  fromMaybe1 :: a -> Maybe a -> a
  fromMaybe1 a Nothing = a
  fromMaybe1 _ (Just a) = a

  listToMaybe1 :: [a] -> Maybe a
  listToMaybe1 [] = Nothing
  listToMaybe1 (x:_) = Just x

  maybeToList :: Maybe a -> [a]
  maybeToList Nothing = []
  maybeToList (Just a) = [a]

  catMaybes1 :: [Maybe a] -> [a]
  catMaybes1 mybs =
    [x | Just x <- mybs]

  flipMaybe :: [Maybe a] -> Maybe [a]
  flipMaybe [] = Just []
  flipMaybe (Nothing:_) = Nothing
  flipMaybe (Just a : rest) =
    case flipMaybe rest of
      Nothing -> Nothing
      Just as -> Just $ a:as

  lefts' :: [Either a b] -> [a]
  lefts' =
    let
      f (Left a) bcc = a:bcc
      f (Right b) bcc = bcc
    in
      foldr f []

  rights' :: [Either a b] -> [b]
  rights' =
    let
      f (Left a) bcc = bcc
      f (Right b) bcc = b:bcc
    in
      foldr f []

  partitionEithers' :: [Either a b] -> ([a], [b])
  partitionEithers' =
    let
      f (Left a) (as,bs) = (a:as,bs)
      f (Right b) (as,bs) = (as,b:bs)
    in
      foldr f ([],[])

  eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe' f (Left a) = Nothing
  eitherMaybe' f (Right b) = Just $ f b

  either' :: (a -> c) -> (b -> c) -> Either a b -> c
  either' fa fb (Left a) = fa a
  either' fa fb (Right b) = fb b

  eitherMaybe1' :: (b -> c) -> Either a b -> Maybe c
  eitherMaybe1' f = either' (const Nothing) (Just . f)

  myIterate :: (a -> a) -> a -> [a]
  myIterate f fir = fir : myIterate f (f fir)

  myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
  myUnfoldr f b =
    case f b of
      Nothing -> []
      Just (a1,b1) -> a1 : myUnfoldr f b1

  betterIterate :: (a -> a) -> a -> [a]
  betterIterate f =
    myUnfoldr (\a -> Just (a, f a))

  unfoldB :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
  unfoldB f a =
    case f a of
      Nothing -> Leaf
      Just (al,b,ar) -> Node (unfoldB f al) b (unfoldB f ar)

  treeBuild :: Integer -> BinaryTree Integer
  treeBuild n
    | n<0 = Leaf
    | otherwise = unfoldB f' 0
      where
        f' x
          | x == n = Nothing
          | otherwise = Just (x+1,x,x+1)

  data Optional a =
    Nada | Only a deriving (Eq, Show)
  instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary =
      frequency [(1, return Nada), (1, Only <$> arbitrary)]
  instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq
  instance Functor Optional where
    fmap f Nada = Nada
    fmap f (Only a) = Only $ f a
-- CH15, MONOID, SEMIGROUP
  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada Nada = Nada
    mappend (Only x) Nada = Only x
    mappend Nada (Only x) = Only x
    mappend (Only a) (Only b) = Only $ mappend a b
  instance Foldable Optional where
    foldMap f Nada = mempty
    foldMap f (Only a) = f a
-- CH21, TRAVERSABLE
  instance Traversable Optional where
    traverse f (Only a) = Only <$> f a
    traverse f Nada = pure Nada
--let trigger = undefined :: Optional (Int, Int, [Int])
--quickBatch (Test.QuickCheck.Classes.traversable trigger)

  newtype First' a =
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

  instance Monoid (First' a) where
    mempty = First' { getFirst' = Nada}
    mappend (First' Nada) (First' b) = First' b
    mappend (First' a) _ = First' a

  firstGen' :: Arbitrary a => Gen (First' a)
  firstGen' = do
    a <- arbitrary
    elements [First' Nada, First' (Only a)]

  instance (Arbitrary a) => Arbitrary (First' a) where
--    arbitrary = firstGen'
    arbitrary = do
      a <- arbitrary
      elements [First' Nada, First' (Only a)]

  firstMappend :: First' a -> First' a -> First' a
  firstMappend = mappend

  type FirstMappend =
    First' String -> First' String -> First' String -> Bool
  type FstId = First' String -> Bool

  semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
  semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
  monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
  monoidLeftIdentity a = (mempty <> a) == a
  monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
  monoidRightIdentity a = (a <> mempty) == a


  data Trivial = Trivial deriving (Eq, Show)
  instance Semigroup Trivial where
    _ <> _ = Trivial
  instance Arbitrary Trivial where
    arbitrary = return Trivial
  type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
  instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

  newtype Identity a = Identity a deriving (Eq, Show, Ord)
  instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity $ a <> b
  instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return $ Identity a
  type IdentityAssoc a = Identity a -> Identity a ->
    Identity a -> Bool
  instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)
  instance Functor Identity where
    fmap g (Identity a) = Identity (g a)
  instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a
  instance Monad Identity where
    return = pure
    Identity a >>= f = f a
  instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq
  instance Foldable Identity where
    foldMap f (Identity a) = f a
-- CH21, TRAVERSABLE
  instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a
--let trigger = undefined :: Identity (Int, Int, [Int])
--quickBatch (Test.QuickCheck.Classes.traversable trigger)

  data Pair a = Pair a a deriving (Eq, Show)
  instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Pair a b
  instance Functor Pair where
    fmap g (Pair x y) = Pair (g x) (g y)
  instance Applicative Pair where
    pure a = Pair a a
    Pair f f1 <*> Pair a a1 = Pair (f a) (f1 a1)
  instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq
--quickBatch $ Test.QuickCheck.Classes.applicative(Pair ("b", "w", 1) ("b", "w", 1))

  data Two a b = Two a b deriving (Eq, Show)
  instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b
  type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
  instance (Monoid a, Monoid b, Semigroup a, Semigroup b) =>
    Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)
  instance Functor (Two a) where
    fmap g (Two a b) = Two a $ g b
  instance (Monoid a, Semigroup a) => Applicative (Two a) where
    pure = Two mempty
    Two a0 f <*> Two a1 b = Two (a0 <> a1) (f b)
  instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq
--quickBatch $ applicative (undefined :: Two String (Int, Double, Char))
  instance Foldable (Two a) where
    foldMap f (Two a b) = f b

  data Three' a b = Three' a b b deriving (Eq, Show)
  instance Functor (Three' a) where
    fmap g (Three' a b0 b1) = Three' a (g b0) (g b1)
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b0 <- arbitrary
      b1 <- arbitrary
      return $ Three' a b0 b1
  instance (Monoid a, Semigroup a) => Applicative (Three' a) where
    pure b = Three' mempty b b
    Three' a0 f0 f1 <*> Three' a1 b0 b1 = Three' (a0 <> a1) (f0 b0) (f1 b1)
  instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq
--quickBatch $ applicative (undefined :: Three' String (Int, Double, Char))
  instance Foldable (Three' a) where
    foldMap f (Three' a b0 b1) = f b0 `mappend` f b1
  instance Traversable (Three' a) where
    traverse f (Three' a b0 b1) =
      liftA2 (Three' a) (f b0) (f b1)

  newtype BoolConj = BoolConj Bool deriving (Eq, Show)
  instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False
  instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj True, BoolConj False]
  type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
  instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

  newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
  instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True
  instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj True, BoolDisj False]
  type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
  instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

  data Or a b = Fst a | Snd b deriving (Eq, Show)
  instance Semigroup (Or a b) where
    (Snd a) <> _ = Snd a
    _ <> (Snd b) = Snd b
    (Fst a) <> (Fst b) = Fst b
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [Fst a, Snd b]
  type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

  newtype Combine a b = Combine { unCombine :: a -> b }
  instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x
  instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
    mempty = Combine $ const mempty
    mappend = (<>)

  newtype Comp a = Comp { unComp :: a -> a }
  instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f.g
  instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

  data Validation a b = Failur a | Succss b deriving (Eq, Show)
  instance Functor (Validation a) where
    fmap f (Failur a) = Failur a
    fmap f (Succss b) = Succss $ f b
  instance (Semigroup a) => Semigroup (Validation a b) where
    (Succss b) <> _ = Succss b
    (Failur a) <> (Succss b) = Succss b
    (Failur a0) <> (Failur a1) = Failur $ a0 <> a1
  instance (Semigroup a) => Applicative (Validation a) where
    pure = Succss
    Succss f <*> Succss b = Succss $ f b
    Failur a <*> Failur b = Failur $ a <> b
    Failur a <*> _ = Failur a
    _ <*> Failur a = Failur a
  instance (Eq a, Eq b) => EqProp (Validation a b) where
    (=-=) = eq
  instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [Failur a, Succss b]
--quickBatch $ Test.QuickCheck.Classes.applicative (Failur ("b",[3],"c"))
  type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

  newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
  instance (Semigroup b) => Semigroup (AccumulateRight a b) where
    (AccumulateRight (Succss b0)) <> (AccumulateRight (Succss b1)) =
      AccumulateRight $ Succss $ b0 <> b1
    (AccumulateRight (Failur a)) <> (AccumulateRight (Succss b)) =
      AccumulateRight $ Failur a
    _ <> (AccumulateRight (Failur a)) = AccumulateRight $ Failur a
  instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (AccumulateRight a b) where
      arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateRight $ Failur a, AccumulateRight $ Succss b]
  type AccmuRAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool

  newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)
  instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Failur a0)) <> (AccumulateBoth (Failur a1)) =
      AccumulateBoth $ Failur $ a0 <> a1
    (AccumulateBoth (Succss b0)) <> (AccumulateBoth (Succss b1)) =
      AccumulateBoth $ Succss $ b0 <> b1
    (AccumulateBoth (Failur a)) <> (AccumulateBoth (Succss b)) =
      AccumulateBoth $ Failur a
    (AccumulateBoth (Succss b)) <> (AccumulateBoth (Failur a)) =
      AccumulateBoth $ Failur a
  instance (Arbitrary a , Arbitrary b) =>
    Arbitrary (AccumulateBoth a b) where
      arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateBoth $ Failur a, AccumulateBoth $ Failur b]
  type AccmuBoAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool

  newtype Mem s a = Mem { runMem :: s -> (a,s) }
  instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)
  instance (Semigroup a) => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \x ->
      let (a0, s0) = f x
          (a1, _) = g x
          a' = a0 <> a1
          (_, s') = g s0
      in (a', s')

  functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIdentity f =
    fmap id f == f

  functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
  functorCompose f g x =
    fmap g (fmap f x) == fmap (g . f) x

  data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
  instance Functor Possibly where
    fmap g (Yeppers a) = Yeppers $ g a
    fmap g LolNope = LolNope

-- P 663
  data Quant a b = Finace | Desk a | Bloor b deriving (Eq, Show)
  instance Functor (Quant a) where
    fmap g (Bloor b) = Bloor $ g b
    fmap _ Finace = Finace
    fmap _ (Desk a) = Desk a

  data K a b = K a deriving (Eq, Show)
  instance Functor (K a) where
    fmap g (K a) = K a

  newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
  instance Functor (Flip K a) where
    fmap g (Flip (K b)) = Flip $ K (g b)

  data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
  instance Functor (EvilGoateeConst a) where
    fmap g (GoatyConst b) = GoatyConst $ g b

  data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
  instance (Functor f) => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut $ fmap g fa

  data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)
  instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap g (DaWrappa fa ga) = DaWrappa (fmap g fa) (fmap g ga)

  data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b) deriving (Eq, Show)
  instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap g (IgnoringSomething fa gb) =
      IgnoringSomething fa (fmap g gb)

  data Notorious g o a t =
    Notorious (g o) (g a) (g t) deriving (Eq, Show)
  instance (Functor g) => Functor (Notorious g o a) where
    fmap g (Notorious go ga gt) =
      Notorious go ga $ fmap g gt

  data List a = Nil | Cons a (List a) deriving (Eq, Show)
  instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = frequency
      [(9,liftA2 Cons arbitrary arbitrary),(1,return Nil)]
  -- OR Cons <$> arbitrary <*> arbitrary
--  instance Monoid (List a) where
--    mempty = Nil
--    mappend = append
  instance Functor List where
    fmap g Nil = Nil
    fmap g (Cons a tail) = Cons (g a) $ fmap g tail
  instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    funcs <*> vals =
      concat' $ fmap flatmapFn funcs where
        flatmapFn f = flatMap (pure.f) vals
--   OR with explicit pattern matching
--    Cons f restf <*> vals =
--      (f <$> vals) `append` (restf <*> vals)
  instance Monad List where
    return = pure
-- does not work: join $ f <$> list
    (>>=) = flip flatMap
-- OR
--  list >>= f = flatMap f list
-- OR
--  Nil >>= f = Nil
--  Cons a rest >>= f = f a `append` (rest >>= f)
  instance (Eq a) => EqProp (List a) where
    (=-=) = eq
--    xs =-= ys = xs' `eq` ys' where
--      xs' = take' 3000 xs
--      ys' = take' 3000 ys
  instance Foldable List where
    foldMap f Nil = mempty
    foldMap f (Cons h t) = f h `mappend` foldMap f t
  instance Traversable List where
    traverse f Nil = pure Nil
    traverse f (Cons h t) = liftA2 Cons (f h) (traverse f t)

  append :: List a -> List a -> List a
  append Nil ys = ys
  append (Cons x xs) ys = Cons x $ xs `append` ys

  fold :: (a -> b -> b) -> b -> List a -> b
  fold _ b Nil = b
  fold f b (Cons h t) = f h (fold f b t)

  concat' :: List (List a) -> List a
  concat' = fold append Nil

  flatMap :: (a -> List b) -> List a -> List b
  flatMap f as = concat' $ f <$> as

  take' :: Int -> List a -> List a
  take' _ Nil = Nil
  take' n (Cons a tail)
    | n <= 0 = Nil
    | otherwise = Cons a $ take' (n-1) tail

  repeatList :: a -> List a
  repeatList x = Cons x $ repeatList x

  newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)
  instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs
  instance Applicative ZipList' where
    pure a = ZipList' $ repeatList a
    ZipList' funcs <*> ZipList' vals =
      ZipList' $ zip' funcs vals where
        zip' _ Nil = Nil
        zip' Nil _ = Nil
        zip' (Cons f ft) (Cons v vt) = Cons (f v) $ zip' ft vt
  instance (Eq a) => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys' where
      xs' = let (ZipList' l) = xs in take' 3000 l
      ys' = let (ZipList' l) = ys in take' 3000 l
  instance (Arbitrary a) => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary
--quickBatch $ Test.QuickCheck.Classes.applicative (ZipList' (Cons ("b", "w", 1) Nil))

  bench f = do
    start <- getCurrentTime
    evaluate f
    end <- getCurrentTime
    print (diffUTCTime end start)

  data GoatLord a =
    NoGoat |
    OneGoat a |
    MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)
  instance Functor GoatLord where
    fmap g NoGoat = NoGoat
    fmap g (OneGoat a) = OneGoat $ g a
    fmap g (MoreGoats a1 a2 a3) =
      MoreGoats (fmap g a1) (fmap g a2) (fmap g a3)

  data TalkToMe a =
    Halt | Print String a | Read {getTalk :: String -> a}
  instance Functor TalkToMe where
    fmap g Halt = Halt
    fmap g (Print str a) = Print str $ g a
    fmap g (Read fsa) = Read $ g.fsa
--getTalk (fmap (+1) (fmap (*4) (Read length))) "ffff"
--getTalk (fmap (+1).(*4) (Read length)) "ffff"
--getF (fmap id (Read length )) "ffff"


-- P686
  newtype Constant a b =
    Constant {getConstant :: a} deriving (Eq, Show, Ord)
  instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary
  instance Functor (Constant a) where
    fmap g (Constant a) = Constant a
  instance (Semigroup a, Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant a0 <*> Constant a1 = Constant $ a0 <> a1
  instance Foldable (Constant a) where
    foldMap f ta = mempty
  instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq
-- CH21, TRAVERSABLE
  instance Traversable (Constant a) where
    traverse f (Constant a) = pure $ Constant a
--let trigger = undefined :: Constant Int (Int, Int, [Int])
--quickBatch (Test.QuickCheck.Classes.traversable trigger)

-- P751
  data Sum1 a b = First1 a | Second1 b deriving (Eq, Show)
  instance Functor (Sum1 a) where
    fmap f (First1 a) = First1 a
    fmap f (Second1 b) = Second1 $ f b
  instance Applicative (Sum1 a) where
    pure = Second1
    First1 a <*> _ = First1 a
    Second1 f <*> r = fmap f r
  instance Monad (Sum1 a) where
    return = pure
    First1 a >>= _ = First1 a
    Second1 b >>= f = f b

-- P763
  data Nope a = NopeDotJpg deriving (Eq, Show)
  instance Functor Nope where
    fmap g NopeDotJpg = NopeDotJpg
  instance Applicative Nope where
    pure _ = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg
  instance Monad Nope where
    return = pure
    NopeDotJpg >>= f = NopeDotJpg
  instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg
  instance EqProp (Nope a) where
    (=-=) = eq

  data PhhhbbtttEither b a = LeftP a | RightP b deriving (Eq, Show)
  instance Functor (PhhhbbtttEither b) where
    fmap f (RightP b) = RightP b
    fmap f (LeftP a) = LeftP $ f a
  instance Applicative (PhhhbbtttEither b) where
    pure = LeftP
    --LeftP f <*> LeftP b = LeftP $ f b
    RightP b <*> _ = RightP b
    LeftP f <*> r = fmap f r
  instance Monad (PhhhbbtttEither b) where
    return = pure
    LeftP a >>= f = f a
    RightP b >>= _ = RightP b
  instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [LeftP a, RightP b]
  instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq
--do
--let trigger = undefined :: Nope (Int, String, Int)
--quickBatch $ functor trigger
--quickBatch $ applicative trigger
--quickBatch $ monad trigger

-- P 809
  sum' :: (Foldable t, Num a) => t a -> a
  sum' xs = getSum $ foldMap Sum xs

  product' :: (Foldable t, Num a) => t a -> a
  product' xs = getProduct $ foldMap Product xs

  elem' :: (Foldable t, Eq a) => a -> t a -> Bool
  elem' a xs = getAny $ foldMap (Any . (a==)) xs

  minimum' :: (Foldable t, Ord a) => t a -> Maybe a
  minimum' = foldr f Nothing where
    f a Nothing = Just a
    f a (Just b) | a < b = Just a
                 | otherwise = Just b

  maximum' :: (Foldable t, Ord a) => t a -> Maybe a
  maximum' = foldr f Nothing where
    f a Nothing = Just a
    f a (Just b) | a > b = Just a
                 | otherwise = Just b

  null' :: (Foldable t) => t a -> Bool
  --null' xs = not.getAny $ foldMap (const $ Any True) xs
  null' xs = getAll $ foldMap (const $ All False) xs

  length' :: (Foldable t) => t a -> Int
  length' xs = getSum $ foldMap (const $ Sum 1) xs

  toList' :: (Foldable t) => t a -> [a]
  toList' = foldMap (:[])

  fold' :: (Foldable t, Monoid m) => t m -> m
  fold' = foldMap id

  foldMap' :: (Foldable t, Monoid m) => (a->m) -> t a -> m
  foldMap' f = foldr (\a b -> f a `mappend` b) mempty

  filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
    (a -> Bool) -> t a -> f a
  filterF f = foldMap g where
    g a = if f a then mempty else pure a
--filterF odd [1..10] :: [Int]
--filterF (>3) [2] :: Maybe (Sum Int)
--filterF (<3) [2] :: Maybe (Sum Int)

  data S n a = S (n a) a deriving (Eq, Show)
  instance (Eq a, Eq (n a)) => EqProp (S n a) where
    (=-=) = eq
  instance (Arbitrary a, CoArbitrary a, Arbitrary (n a)) =>
    Arbitrary (S n a) where
    arbitrary = do
      n <- arbitrary
      a <- arbitrary
      return $ S (n a) a
  instance (Monoid a, Monoid (n a)) => Monoid (S n a) where
    mempty = S mempty mempty
    mappend (S na0 a0) (S na1 a1) =
      S (na0 `mappend` na1) (a0 `mappend` a1)
  instance (Functor n) => Functor (S n) where
    fmap f (S na a) = S (f <$> na) (f a)
  instance (Applicative n) => Applicative (S n) where
    pure a = S (pure a) a
    S nf f <*> S na a = S (nf <*> na) (f a)
--  instance (Foldable n, Applicative n) =>
--    Monad (S n) where
--    S n a >>= f = nb1 `mappend` f a where
--      nb = f <$> n
--      nb1 = foldMap id nb
--  FAILED TO WRITE MONAD INSTANCE
  instance (Foldable n) => Foldable (S n) where
    foldMap f (S n a) = foldMap f n `mappend` f a
  instance (Traversable n) => Traversable (S n) where
    traverse f (S n a) = liftA2 S (traverse f n) (f a)

  data Tree a = EmptyT | LeafT a | NodeT (Tree a) a (Tree a)
    deriving (Eq, Show)
  instance (Eq a) => EqProp (Tree a) where
    (=-=) = eq
  instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = frequency
      [(2, return EmptyT), (1, LeafT <$> arbitrary),
      (3, liftA3 NodeT arbitrary arbitrary arbitrary)]
  instance Functor Tree where
    fmap f EmptyT = EmptyT
    fmap f (LeafT a) = LeafT (f a)
    fmap f (NodeT l a r) = NodeT (f<$>l) (f a) (f<$>r)
  instance Foldable Tree where
    foldMap f EmptyT = mempty
    foldMap f (LeafT a) = f a
    foldMap f (NodeT l a r) =
      foldMap f l `mappend` f a `mappend` foldMap f r
  instance Traversable Tree where
    traverse f EmptyT = pure EmptyT
    traverse f (LeafT a) = LeafT <$> f a
    traverse f (NodeT l a r) =
      liftA3 NodeT (traverse f l) (f a) (traverse f r)

  newtype Reader r a = Reader { runReader :: r -> a }
  instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f.ra
  instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const
--  pure a = Reader $ const a
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) =
      Reader $ \r -> rab r (ra r)
  instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    Reader ra >>= aRb =
      Reader $ \r -> (runReader $ aRb (ra r)) r
--    join $ Reader $ \r -> aRb (ra r)  -- WOW

  newtype HumanName = HumanName String deriving (Eq, Show)
  newtype DogName = DogName String deriving (Eq, Show)
  newtype Address = Address String deriving (Eq, Show)
  data Person = Person {humanName :: HumanName,
                        dogName :: DogName,
                        address :: Address }
    deriving (Eq, Show)
  data Dog = Dog {dogsName :: DogName,
                  dogsAddress :: Address }
    deriving (Eq, Show)

  getDogR'' :: Reader Person Dog
  getDogR'' = Dog <$> Reader dogName <*> Reader address
--getDogR'' = Reader $ Dog <$> dogName <*> address

--flip TO MATCH TYPES FOR MONAD INSTANCE
  getDogRM :: Reader Person Dog
  getDogRM = Reader $ address >>= flip (Dog <$> dogName)

--flip :: (a -> b -> c) -> b -> a -> c
--(Dog <$> Reader dogName) :: Reader Person (Address -> Dog)
--SO NEED TO USE CUSTOM flipReader FOR NESTED Reader BELOW
  getDogRM' :: Reader Person Dog
  getDogRM' =
    Reader address >>= flipReader (Dog <$> Reader dogName)

  flipReader :: Reader a (b -> c) -> b -> Reader a c
  flipReader (Reader f) = Reader . flip f
--flipReader (Reader f) = \b -> Reader $ flip f b
