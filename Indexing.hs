{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

-- | Monotone hash based index.
module Indexing (pad, openIndex, closeIndex, Index, IndexingError(..), QueryOptions(..), index, MemoryIndex, readIndex, lookUp) where

import System.Random
import Control.Monad.Random
import System.IO
import Control.Exception
import Control.Monad.Loops
import Control.Monad
import Data.Array.IArray hiding (index)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Data.Typeable
import Data.Function
import Data.ByteString.Char8 (ByteString, unpack, hGet)
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Prelude hiding (catch)

import Unpacks

import System.IO.Unsafe

pad _ 0 ls = ls
pad with n [] = replicate n with
pad with n (x:xs) = x : pad with (n - 1) xs

windows sz ls = pad '\0' sz (take sz ls) : if null dr then [] else windows sz dr where
	dr = drop 5 ls

bits :: Array Int [Int]
bits = listArray (0, 255) $ evalRand (mapM (\_ -> mapM (\_ -> getRandomR (0, 15)) [(), ()]) [0..255]) (mkStdGen 0)

nBits n = 1 + maximum (filter (testBit n) [31,30..0])

hashByte nBits hash x = if x == '\0' || ord x > 255 then
		hash
	else
		foldl setBit hash $ take nBits $ bits ! ord x

hashWindow :: Int -> String -> Int32
hashWindow bits s = foldl (hashByte n) (foldl (hashByte (n + 1)) 0 tk) dr where
	(n, r) = quotRem bits 10
	(tk, dr) = splitAt r s

choose _ [] = [[]]
choose 0 _ = [[]]
choose n xs = concatMap (\(x:xs) -> map (x:) (choose (n - 1) xs)) (take (length xs - n + 1) (tails xs))

compatiblePlaces1 :: Int -> String -> [Int32]
compatiblePlaces1 nBits window = map (foldl setBit hash) $ choose (((nBits - popCount hash) `min` length bits) `max` 0) bits where
	hash = hashWindow nBits window
	bits = filter (not . testBit hash) [0..15]

compatiblePlaces :: Int -> String -> [(Int32, Int32)]
compatiblePlaces sizeCode window = concatMap (\code -> concat $ zipWith (\n -> map ((,) (fromIntegral n)) . compatiblePlaces1 code . take 10 . (++window))
	[4,3..0]
	(tails $ replicate 4 '\0'))
	[5..sizeCode]

superblockSize :: Int32
superblockSize = 2 ^ 20 * 32

openIndex path overflow = do
	hdl <- openBinaryFile path ReadWriteMode
	sz <- hFileSize hdl
	hSetFileSize hdl (sz `max` toInteger (superblockSize + 128)) -- 32 mebibytes + dummy entry
	overflowHdl <- openBinaryFile overflow ReadWriteMode
	return (Index hdl overflowHdl)

closeIndex (Index hdl overflow) = hClose hdl >> hClose overflow

data Index = Index !Handle !Handle deriving Eq

data IndexingError = PathTooLong -- a path was too long
	| ResourceError deriving (Show, Typeable) -- an internal error occurred

instance Exception IndexingError

data QueryOptions = QueryOptions { caseSensitive :: !Bool, inArchives :: !Bool }

{-# NOINLINE buffer #-}
buffer = unsafePerformIO (new 0)

hGetInt32 :: Handle -> IO Int32
hGetInt32 hdl = do { hGetBuf hdl buffer 4; peek buffer }

hPutInt32 :: Handle -> Int32 -> IO ()
hPutInt32 hdl x = do { poke buffer x; hPutBuf hdl buffer 4 }

seekForWindow idx i j = hSeek idx AbsoluteSeek $ toInteger $ 2 ^ 9 * i + 8 * j

printable x = ord x >= 32 && ord x <= 255 || ord x == 10 || ord x == 13

interesting = any (`notElem` "\t\n\r ")

data MemoryIndex = MemoryIndex !Index !(UArray (Int32, Int32) Int32) !(Array Int32 ByteString)

sizeToBits :: Integer -> Int
sizeToBits = max 0 . (20 -) . nBits . (`quot` 128) . subtract superblockSize . fromInteger

readIndex idx@(Index hdl _) = do
	sz <- hFileSize hdl
	hSeek hdl AbsoluteSeek 0
	ls <- mapM (\_ -> hGetInt32 hdl) [0..2 ^ 23 - 1]
	ls2 <- fix $ \next -> do
		b <- hIsEOF hdl
		if b then
				return []
			else
				liftM2 (:) (hGet hdl 128) next
	return $! MemoryIndex idx (listArray rng ls) (listArray (0, (fromInteger sz - superblockSize) `quot` 128) ls2) where
	rng = ((0, 0), (2 ^ 16 - 1, 2 ^ 7 - 1))

index :: Index -> FilePath -> String -> IO ()
index (Index idx overflow) path contents = do
	-- Check path length
	when (length path > 128) $ throwIO PathTooLong

	-- Add the path to the path array
	hSeek idx SeekFromEnd 0
	pathI <- liftM ((`quot` 128) . subtract superblockSize . fromInteger) (hTell idx)
	hPutStr idx (pad '\0' 128 path)

	sz <- hFileSize idx

	-- Index the file
	foldr (\(i, window) next1 -> when (interesting window) $
		-- Search for a place
		foldr (\k next -> do
			-- Seek to the point in the file where the window should go
			seekForWindow idx k 0

			foldr (\_ next0 -> do
				pathI1 <- hGetInt32 idx
				j <- hGetInt32 idx
				if pathI1 == 0 then do
						hSeek idx RelativeSeek (-8)
						hPutInt32 idx pathI
						hPutInt32 idx i
						next1
					else if pathI1 == pathI && j == i then -- Prevent duplicates
						next1
					else
						next0)
				next
				[0..2 ^ 6 - 1])
			(do
			putStrLn "(Overflow)"
			hSeek overflow SeekFromEnd 0
			hPutStr overflow $ pad '\0' 128 path)
			$ compatiblePlaces1 (sizeToBits sz) window)
		(return ())
		$ zip [0,5..] (windows 10 $ map toUpper $ takeWhile printable contents)

getFile' options path = if inArchives options || '@' `notElem` path then
		getFile path
	else
		return nullDevice

lookUp :: MemoryIndex -> QueryOptions -> String -> IO [(FilePath, Int32)]
lookUp (MemoryIndex (Index hdl overflow) superblock remainder) options string = do
	let string1 = (if caseSensitive options then id else map toUpper) string
	sz <- hFileSize hdl
	buf <- mallocBytes 128
	finally
		-- Get positions for the string
		(do
		let positions = map (\ls -> (fst (head ls), map snd ls)) $ groupBy ((==) `on` fst) $ sort $ concatMap (\(offset, i) ->
			foldr (\j next ->
			let
				pathI = superblock ! (i, 2 * j)
				k = superblock ! (i, 2 * j + 1)
				path = reverse $ dropWhile (=='\0') $ reverse $ unpack $ remainder ! pathI in
			if pathI == 0 then
					[]
				else
					(path, k + offset) : next)
			[]
			[0..2 ^ 6 - 1])
			$ compatiblePlaces (sizeToBits sz) string1

		-- Inspect files at the stated positions for the string
		res <- mapM (\(path, positions) -> catch
			(do
				path' <- getFile' options path
				inxd <- openBinaryFile path ReadMode
				finally (foldr (\pos next -> do
					hSeek inxd AbsoluteSeek $ toInteger pos
					s <- hGet inxd (length string1)
					if string1 == (if caseSensitive options then id else map toUpper) (unpack s) then
							return [(path, pos)]
						else
							next)
					(return [])
					positions)
					(hClose inxd))
			(\(_ :: IOError) -> return []))
			positions

		-- Read the overflow file
		hSeek overflow AbsoluteSeek 0
		res2 <- unfoldM $ do
			b <- hIsEOF overflow
			if b then
					return Nothing
				else do
					path <- liftM (reverse . dropWhile (=='\0') . reverse . unpack) $ hGet overflow 128
					path' <- getFile' options path
					inxd <- openBinaryFile path' ReadMode
					contents <- hGetContents inxd
					return $! Just $! case findIndex (isPrefixOf string1) $ tails $ (if caseSensitive options then id else map toUpper) contents of
						Just k -> [(path, fromIntegral k)]
						Nothing -> []
		return $ concat $ res2 ++ res)
		(free buf)
