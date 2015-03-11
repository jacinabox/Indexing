{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

-- | Monotone hash based index.
module Indexing (pad, openIndex, closeIndex, Index, IndexingError(..), index, lookUp) where

import System.Random
import Control.Monad.Random
import System.IO
import Control.Exception
import Control.Monad.Loops
import Control.Monad
import Data.Array hiding (index)
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Data.Typeable
import Data.ByteString.Char8 (unpack, hGet)
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Prelude hiding (catch)

pad _ 0 ls = ls
pad with n [] = replicate n with
pad with n (x:xs) = x : pad with (n - 1) xs

windows sz ls = pad '\0' sz (take sz ls) : if null dr then [] else windows sz dr where
	dr = drop 5 ls

bits :: Array Int Int
bits = listArray (0, 256 * 10 - 1) $ evalRand (mapM (\_ -> getRandomR (0, 15)) [0..256 * 10 - 1]) (mkStdGen 0)

hashByte (hash, i) x = (if x == '\0' then
		hash
	else
		setBit hash (bits ! (256 * i + ord x)),
	i + 1)

hashWindow :: String -> Int32
hashWindow = fst . foldl hashByte (0, 0)

nBits x = sum $ map ((.&. 1) . shiftR x) [0..15]

-- Put the bits in x in the 1 positions in mask
interlace mask x = if x == 0 || mask == 0 then
		0
	else if testBit mask 0 then
		(x .&. 1) .|. shiftL (interlace (shiftR mask 1) (shiftR x 1)) 1
	else
		shiftL (interlace (shiftR mask 1) x) 1

compatiblePlaces1 window = map ((.|. hash) . interlace bits) [0..2 ^ nBits bits - 1] where
	hash = hashWindow window
	bits = (2 ^ 16 - 1) .&. complement hash

compatiblePlaces window = concat $ zipWith (\n -> map ((,) n) . compatiblePlaces1 . take 10 . (++window)) [4,3..0] (tails (replicate 4 '\0'))

superblockSize :: Int32
superblockSize = 2 ^ 20 * 32

openIndex path overflow = do
	hdl <- openBinaryFile path ReadWriteMode
	sz <- hFileSize hdl
	hSetFileSize hdl (sz `max` toInteger (superblockSize + 128)) -- 32 mebibytes + dummy entry
	overflowHdl <- openBinaryFile overflow ReadWriteMode
	return (Index hdl overflowHdl)

closeIndex (Index hdl overflow) = hClose hdl >> hClose overflow

data Index = Index Handle Handle deriving Eq

data IndexingError = PathTooLong -- a path was too long
	| ResourceError deriving (Show, Typeable) -- an internal error occurred

instance Exception IndexingError

hGetInt32 :: Handle -> IO Int32
hGetInt32 hdl = do
	p <- new 0
	finally
		(do { hGetBuf hdl p 4; peek p })
		(free p)

hPutInt32 :: Handle -> Int32 -> IO ()
hPutInt32 hdl x = do
	p <- new 0
	finally
		(do { poke p x; hPutBuf hdl p 4 })
		(free p)

seekForWindow hdl loc place = hSeek hdl AbsoluteSeek (toInteger $ 2 ^ 9 * loc + 8 * place)

printable x = ord x >= 32 && ord x <= 255 || ord x == 10 || ord x == 13

interesting = any (`notElem` "\t\n\r ")

index :: Index -> FilePath -> String -> IO ()
index (Index idx overflow) path contents = do
	-- Check path length
	when (length path > 128) $ throwIO PathTooLong

	catch (do
		-- Add the path to the path array
		hSeek idx SeekFromEnd 0
		pathI <- liftM ((`quot` 128) . subtract superblockSize . fromInteger) (hTell idx)
		hPutStr idx (pad '\0' 128 path)

		-- Index the file
		foldr (\(i, window) next1 -> when (interesting window) $ do
			-- Search for a place
			foldr (\i next -> do
				-- Seek to the point in the file where the window should go
				seekForWindow idx i 0

				foldr (\i next0 -> do
					pathI1 <- hGetInt32 idx
					if pathI1 == 0 then do
							hSeek idx RelativeSeek (-4)
							hPutInt32 idx pathI
							hPutInt32 idx i
							next1
						else do
							hGetInt32 idx
							next0)
					next
					[0..2 ^ 6 - 1])
				(do
				hSeek overflow SeekFromEnd 0
				hPutStr overflow (pad '\0' 128 path))
				(compatiblePlaces1 window))
			(return ())
			$ zip [0,5..] (windows 10 $ map toUpper $ takeWhile printable contents))
		(\(_ :: IOError) -> throwIO ResourceError)

lookUp :: Index -> Bool -> String -> IO [(FilePath, Int32)]
lookUp (Index hdl overflow) caseSensitive string = catch
	(do
	let string1 = (if caseSensitive then id else map toUpper) string
	buf <- mallocBytes 128
	finally
		-- Inspect each file at the stated position for the string
		(do
		res <- mapM (\(offset, i) ->
			foldr (\j next -> do
			-- Seek to the window position
			seekForWindow hdl i j

			pathI <- hGetInt32 hdl
			k <- hGetInt32 hdl

			-- Get the path
			hSeek hdl AbsoluteSeek (toInteger $ superblockSize + 128 * pathI)
			path <- liftM (reverse . dropWhile (=='\0') . reverse . unpack) $ hGet hdl 128

			-- Check if the string is at the stated position
			if pathI == 0 then
					return []
				else
				liftM2 (++) (catch
				(do
					inxd <- openBinaryFile path ReadMode
					finally (do
						hSeek inxd AbsoluteSeek (toInteger k + toInteger offset)
						liftM (\s -> if string1 == (if caseSensitive then id else map toUpper) (unpack s) then [(path, k + fromIntegral offset)] else []) $ hGet inxd (length string1))
						(hClose inxd))
				(\(_ :: IOError) -> return []))
				next)
			(return [])
			[0..2 ^ 6 - 1])
			$ compatiblePlaces string1

		-- Read the overflow file
		hSeek overflow AbsoluteSeek 0
		res2 <- unfoldM $ do
			b <- hIsEOF overflow
			if b then
					return Nothing
				else do
					path <- liftM (reverse . dropWhile (=='\0') . reverse . unpack) $ hGet overflow 128
					inxd <- openBinaryFile path ReadMode
					contents <- hGetContents inxd
					return $ Just $ case findIndex (isPrefixOf string1) $ tails $ (if caseSensitive then id else map toUpper) contents of
						Just k -> [(path, fromIntegral k)]
						Nothing -> []
		return $ concat res2 ++ nub (concat res))
		(free buf))
	(\(_ :: IOError) -> throwIO ResourceError)
