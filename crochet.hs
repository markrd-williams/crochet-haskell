{-# LANGUAGE MultiWayIf #-}
module Crochet where

import Data.Char (isAlpha, isNumber)
import Data.List (delete, union, nub, inits, minimumBy)
import System.IO
import Text.ParserCombinators.ReadP

data CrochetCode
  = NoOp
  | SC
  | Seq Int CrochetCode
  | Inc Int
  | Dec Int
  | Then CrochetCode CrochetCode
  deriving (Eq)

instance Semigroup CrochetCode where
  (<>) = Then


instance Show CrochetCode where
  show :: CrochetCode -> String
  show NoOp = ""
  show SC = "SC"
  show (Inc n) = "Increase " ++ show n
  show (Dec n) = "Decrease " ++ show n
  show (Then c1 c2) = show c1 ++ "\n" ++ show c2
  show (Seq n c) = "Seq " ++ show n ++ ":\n  " ++ concatMap (\s -> if s == '\n' then "\n  " else pure s) (show c)


size :: CrochetCode -> Int
size NoOp         = 0
size SC           = 1
size (Inc n)      = 1
size (Dec n)      = 1
size (Seq n c)    = 1 + size c
size (Then c1 c2) = size c1 + size c2 + 10000

unthen :: CrochetCode -> [CrochetCode]
unthen (Then c1 c2) = unthen c1 ++ unthen c2
unthen c = [c]

thenReduce :: CrochetCode -> CrochetCode -> [CrochetCode]
thenReduce NoOp c = [c]
thenReduce c NoOp = [c]
thenReduce c c' | c == c' = [Seq 2 c]
thenReduce (Seq n c) (Seq m c') | c == c' = [Seq (n + m) c]
thenReduce (Seq n c) (Seq m c') | n == m = [Seq n (c <> c')]
thenReduce (Seq n c) c' | c == c' = [Seq (n + 1) c]
thenReduce c' (Seq n c) | c == c' = [Seq (n + 1) c]
thenReduce (Inc n) (Inc m) = [Inc (n + m)]
thenReduce (Dec n) (Dec m) = [Dec (n + m)]
thenReduce c c' = [c, c']

minimumByKey :: (Ord b, Foldable t) => (a -> b) -> t a -> a
minimumByKey f = minimumBy (\x y -> f x `compare` f y)

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : [ (x : before, after) | (before, after) <- splits xs ]

thenJoin :: [CrochetCode] -> CrochetCode
thenJoin [] = NoOp
thenJoin xs = foldl1 (<>) xs



thenReduce' :: CrochetCode -> [CrochetCode] -> [CrochetCode]
thenReduce' c [] = [c]
thenReduce' c (x : xs) = thenReduce c x ++ xs

-- thenReduce'' :: [CrochetCode] -> CrochetCode
-- thenReduce'' xs = map (minimumByKey size . (\(h, t) -> thenReduce' (thenJoin h) t)) (splits xs)

norm :: CrochetCode -> CrochetCode
norm NoOp = NoOp
norm SC = SC
norm (Inc 0) = NoOp
norm (Inc n) = Inc n
norm (Dec 0) = NoOp
norm (Dec n) = Dec n
norm (Then c1 c2) =
  foldr1 (<>) (foldr thenReduce' [] un)
  where
    un = unthen (norm c1) ++ unthen (norm c2)
norm (Seq 0 c) = NoOp
norm (Seq 1 c) = norm c
norm (Seq n c) =
  case norm c of
    NoOp -> NoOp
    (Seq m c') -> Seq (n * m) c'
    c' -> Seq n c'

evenSpacing :: Int -> Int -> [Int]
evenSpacing m n = [i * n `div` m + n `div` (2 * m) | i <- [0..m-1]]


crochetn :: Int -> CrochetCode
crochetn x
  | x == 0 = Dec 1
  | x == 1 = SC
  | x > 1  = SC <> Inc (x - 1)
  | x < 0  = NoOp

crochetThese :: [Int] -> CrochetCode
crochetThese = foldr ((<>) . crochetn) NoOp

listToBag :: [Int] -> Int -> [Int]
listToBag [] n = replicate n 0
listToBag (x:xs) n = take x b ++ [(b !! x) + 1] ++ drop (x + 1) b
  where
    b = listToBag xs n

nextRow :: Int -> Int -> CrochetCode
nextRow current goal = crochetThese $ listToBag (evenSpacing goal current) current

nextRow' :: Int -> Int -> CrochetCode
nextRow' current goal =
  if goal == 0
    then NoOp
    else case
     current `compare` goal of
      EQ -> Seq current SC
      LT ->
        let per_hole = goal `div` current
            extra = goal `mod` current
         in case extra `compare` 0 of
              LT -> undefined
              EQ -> Seq current $ SC <> Inc (per_hole - 1)
              GT ->
                let st_spacing = current `div` extra
                    extra_end = goal `mod` extra
                 in
                      Seq
                        extra
                        (Seq (st_spacing - 1) (SC <> Inc (per_hole - 1)) <> SC <> Inc per_hole)
                        <> Seq extra_end (SC <> Inc extra_end)
      GT ->
        let per_stitch = current `div` goal - 1
            extra = current `mod` goal
         in case extra `compare` 0 of
              LT -> undefined
              EQ -> Seq goal (Dec per_stitch <> SC)
              GT ->
                let st_spacing = goal `div` extra
                    extra_end = goal `mod` extra
                 in Seq extra (Seq (st_spacing - 1) (Dec per_stitch <> SC) <> Dec (per_stitch + 1) <> SC) <> Dec extra_end <> SC

neighbours :: [(Float, Float, Float)] -> [(Int, Int, Int)] -> [[Int]]
neighbours vs [] = replicate (length vs) []
neighbours vs ((i, j, k) : ts) =
  zipWith
    ( \l s ->
        if l == i || l == j || l == k
          then s `union` delete l [i, j, k]
          else s
    )
    [0 ..]
    (neighbours vs ts)


outerBoundary :: [Int] -> [Int] -> [[Int]] -> [Int]
outerBoundary b s ns = nub [n | i <- b, n <- ns !! i, n `notElem` s]

instructions :: [[Int]] -> CrochetCode
instructions nbs = norm $ go [1] [1]
    where
        go :: [Int] -> [Int] -> CrochetCode
        go tot []   = NoOp
        go tot bdry = Then (nextRow current_stitches goal_stitches) (go (tot `union` next_row) next_row)
            where
                current_stitches :: Int
                current_stitches = length bdry

                next_row :: [Int]
                next_row = outerBoundary bdry tot nbs

                goal_stitches :: Int
                goal_stitches = length next_row



-- TODO: Fix types?
triple :: [a] -> (a, a, a)
triple [x, y, z] = (x, y, z)

parseVertex :: String -> (Float, Float, Float)
parseVertex = triple . map read . take 3 . words

parseTriangle :: String -> (Int, Int, Int)
parseTriangle = triple . map read . take 3 . drop 1 . words

main :: IO ()
main = do
  f <- readFile "/home/mark/Documents/hackathon/crochet/data/untitled.off"
  let lines_of_f = filter (not . null) (lines f)
  let (n_verts : n_faces : n_dontknow : _) :: [Int] = map read $ words (lines_of_f !! 1)
  let vs = map parseVertex $ take n_verts (drop 2 lines_of_f)
  let ts = map parseTriangle $ take n_faces (drop (2 + n_verts) lines_of_f)
  let nbs = neighbours vs ts
  print (instructions nbs)