{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

-- | Monotone hash based index.
module Indexing (pad, openIndex, closeIndex, Index, IndexingError(..), QueryOptions(..), index, MemoryIndex, readIndex, lookUp) where

import System.Random
import Control.Monad.Random
import System.IO
import Control.Exception
import Control.Monad.Loops
import Control.Monad
import Control.Parallel.Strategies
import Control.Concurrent
import Data.Array.IArray hiding (index)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Word
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

import System.IO.Unsafe

import LazySequence
import Unpacks

pad _ 0 ls = ls
pad with n [] = replicate n with
pad with n (x:xs) = x : pad with (n - 1) xs

windows0 sz ls = take sz ls : if null dr then [] else windows0 sz dr where
	dr = drop sz ls

windows sz = map (pad '\0' sz) . windows0 sz

bits :: Array Int [Int]
bits = listArray (0, 255) $ evalRand (mapM (\_ -> mapM (\_ -> getRandomR (0, 15)) [(), ()]) [0..255]) (mkStdGen 0)

hashByte f hash x = if x == '\0' || ord x > 255 then
		hash
	else
		foldl f hash $ bits ! ord x

hashWindow :: Word16 -> String -> Word16
hashWindow init wnd = foldl (hashByte clearBit) (foldl (hashByte setBit) init tk) dr where
	(tk, dr) = splitAt 5 wnd

choose [] = [[]]
choose (x:xs) = map (x:) chs ++ chs where chs = choose xs

compatiblePlaces :: String -> [Word16]
compatiblePlaces window = map (foldl setBit hash2) $ choose bits where
	hash1 = hashWindow maxBound window
	hash2 = hashWindow 0 window
	hash = hash1 `xor` hash2
	bits = filter (testBit hash) [0..15]

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

seekForWindow :: Handle -> Int32 -> Int32 -> IO ()
seekForWindow idx i j = hSeek idx AbsoluteSeek $ toInteger $ 2 ^ 9 * i + 8 * j

printable x = ord x >= 32 && ord x <= 255 || ord x == 10 || ord x == 13

interesting = any (`notElem` "\t\n\r ")

data MemoryIndex = MemoryIndex !Index !(UArray (Int32, Int32) Int32) !(Array Int32 ByteString)

readIndex idx@(Index hdl _) = do
	setNumCapabilities 8

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

packInt :: [Int32] -> Int32
packInt [x1,x2,x3,x4] = shiftL x1 24 .|. shiftL x2 16 .|. shiftL x3 8 .|. x4

-- Squeezes the important info in a string into 16 bits
packInt1 :: [Int32] -> Int32
packInt1 [x1,x2,x3,x4] = shiftL x1 12 .|. shiftL x2 8 .|. shiftL x3 4 .|. x4

unpackInt :: Int32 -> [Int32]
unpackInt i = [shiftR i 24, shiftR i 16 .&. 255, shiftR i 8 .&. 255, i .&. 255]

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
	foldr (\(i, window) next1 -> when (interesting window) $ do
		-- Search for a place to index the word sequence
		let (tk, x1:x2:rest) = splitAt 4 window
		let n = packInt1 (map (fromIntegral . ord) tk) `mod` 65536
		let entry1 = packInt [-1,0,fromIntegral (ord x1),fromIntegral (ord x2)]
		let entry2 = packInt $ map (fromIntegral . ord) rest
		seekForWindow idx n 0
		foldr (\_ next0 -> do
			i1 <- hGetInt32 idx
			i2 <- hGetInt32 idx
			if i1 == 0 then do
					hSeek idx RelativeSeek (-8)
					hPutInt32 idx entry1
					hPutInt32 idx entry2
				else if i1 /= entry1 || i2 /= entry2 then
					next0
				else
					return ())
			(return ())
			[0..2 ^ 6 - 1]

		-- Search for a place to index the document
		foldr (\k next -> do
			-- Seek to the point in the file where the window should go
			seekForWindow idx (fromIntegral k) 0

			foldr (\loc next0 -> do
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
			$ compatiblePlaces window)
		(return ())
		$ zip [0,10..] (windows 10 $ map toUpper $ takeWhile printable contents)

getFile' options path = if inArchives options || '@' `notElem` path then
		getFile path
	else
		return nullDevice

getExtensions superblock string1 = if length string1 >= 10 then
		[string1]
	else
		filter (string1 `isPrefixOf`) $ foldr (\j next ->
			let
				i1 = superblock ! (i, 2 * j)
				i2 = superblock ! (i, 2 * j + 1) in
			if i1 == 0 then
				[]
			else if i1 > 0 then
				next
			else let
				[_, _, x1, x2] = unpackInt i1
				xs = unpackInt i2 in
				(tk ++ map (chr . fromIntegral) (x1 : x2 : xs)) : next)
		[]
		[0..2 ^ 6 - 1] where
	tk = take 4 string1
	i = packInt1 (map (fromIntegral . ord) tk) `mod` 65536

bigZip ls = if any null ls then [] else map head ls : bigZip (map tail ls)

parConcat :: [[t]] -> [t]
parConcat ls = concat $ concat (bigZip (map (windows0 4096) ls) `using` parList (evalList rseq))

lookUp0 :: MemoryIndex -> Bool -> String -> [(FilePath, Int32)]
lookUp0 (MemoryIndex _ superblock remainder) padded string1 = let
	-- Get extensions of the search string
		strings0 = getExtensions superblock string1
		strings = if padded then
				concatMap (\s -> map (\n -> (fromIntegral n, pad '\0' n s)) [0..4]) strings0
			else
				map ((,) 0) strings0 in

	-- Get positions for the string
	concatMap (\(offset, s) -> concatMap (\i ->
		foldr (\j next ->
		let
			pathI = superblock ! (fromIntegral i, 2 * j)
			k = superblock ! (fromIntegral i, 2 * j + 1)
			path = reverse $ dropWhile (=='\0') $ reverse $ unpack $ remainder ! pathI in
		if pathI <= 0 then
				[]
			else
				(path, k + offset) : next)
		[]
		[0..2 ^ 6 - 1])
		$ compatiblePlaces
		$ take 10 s)
		strings

lookUp mem@(MemoryIndex (Index hdl overflow) _ _) options string = do
	let string1 = (if caseSensitive options then id else map toUpper) string
	sz <- hFileSize hdl
	buf <- mallocBytes 128
	finally
		(do
		let positions = parConcat $ zipWith (lookUp0 mem) (True : repeat False) (take (5 `min` (length string1 - 3)) $ tails string1)

		-- Inspect files at the stated positions for the string
		res <- lazyMapM
			(nubBy ((==) `on` fst) . concat)
			(\(path, position) -> catch
				(do
				path' <- getFile' options path
				inxd <- openBinaryFile path ReadMode
				finally (do
					hSeek inxd AbsoluteSeek $ toInteger position
					s <- hGet inxd (length string1)
					return $ if string1 == (if caseSensitive options then id else map toUpper) (unpack s) then
							[(path, position)]
						else
							[])
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
					catch (do
						path' <- getFile' options path
						inxd <- openBinaryFile path' ReadMode
						contents <- hGetContents inxd
						return $! Just $! case findIndex (isPrefixOf string1) $ tails $ (if caseSensitive options then id else map toUpper) contents of
							Just k -> [(path, fromIntegral k)]
							Nothing -> [])
						(\(_ :: IOError) -> return $! Just [])

		return $ res ++ concat res2)
		(free buf)
