-- | Index based on compressibility of the Burroughs-Wheeler Transform.
module Indexing where

import Data.List
import Control.Arrow
import Data.Array

fromList ls = listArray (0, ls - 1) ls

lAndF :: String -> (Array Int Char, Array Int Char)
lAndF = (fromList . map head &&& fromList . map last) . sort . init . tails

c :: Char -> Array Int Char -> Int
c c f = binSearch (bounds f) where
	binSearch (lo, hi) = if hi <= lo then
			lo
		else if f ! avg < c then
			binSearch (avg, hi)
		else
			binSearch (lo, avg) where
	avg = (hi - lo) `quot` 2 + lo

occ0 :: Char -> Array Int Char -> [Int]
occ0 c = scanl (\n c2 -> if c == c2 then succ n else n) 0

windows _ [] = []
windows n ls = tk : windows n dr where
	(tk, dr) = splitAt n ls

occ l = listArray (ord 0, ord 255) $ map (\c -> listArray (0, snd (bounds l) `quot` 16) $ map head $ windows 16 $ occ0 c l) [ord 0..ord 255]

countOccs occ c pos = occ ! c ! (d + 1) + play through the text where
	(d, r) = divMod pos 16

lf0 c occ (l, f) i = c ! ch + countOccs occ ch i where
	ch = l ! i

lf (l, f) = listArray (bounds l) $ map (lf0 (l, f)) (range (bounds l))

runs f = fmap (! snd (bounds f)) $ occ f
