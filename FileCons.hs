{-# LANGUAGE CPP, ScopedTypeVariables #-}

module FileCons (Cons, openHandle, closeHandle, newCons, newInt, isPair, first, second, setFirst, setSecond, int, list, toList, nth, shw, cmpr, cmpr2, encodeString, decodeString, dlookup, insert, deleteFindMin, deleteFindMax, delete, depth, size) where

import System.IO.Unsafe
import Control.Monad
import Data.Bits
import Data.Char
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO
import Control.Exception
import System.Directory
import Prelude hiding (catch)

-- This module stores a single value in a file. The value is a tree structure like Lisp's
-- conses. The value in the file can be changed while sharing structure with values
-- that were once in the file.

hReadInt :: Handle -> IO Int
hReadInt hdl = do
	for <- mallocForeignPtr
	withForeignPtr for $ \p -> do
		hGetBuf hdl (castPtr p) 4
		peek p

hWriteInt :: Handle -> Int -> IO ()
hWriteInt hdl n = do
	for <- mallocForeignPtr
	withForeignPtr for $ \p -> do
		poke p n
		hPutBuf hdl (castPtr p) 4
	return ()

data Cons = Cons !Handle !Int deriving Eq

openHandle path mode = do
	exists <- doesFileExist path
	hdl <- openBinaryFile path mode
	unless exists $ do
		hWriteInt hdl (-1)
		hWriteInt hdl (-1)
	return (Cons hdl 0)

-- Closes a file. Values that depended on the handle will give errors
-- if their operations are used.
closeHandle (Cons hdl _) = hClose hdl

-- Functions for building and taking apart values.

newCons (Cons hdl i) (Cons hdl2 j)
#ifdef DEBUG
	| hdl /= hdl2	= error "newCons: have to come from same file"
#endif
	| otherwise	= unsafePerformIO $ do
		hSeek hdl SeekFromEnd 0
		pos <- hTell hdl
		hWriteInt hdl i
		hWriteInt hdl j
		return (Cons hdl (fromIntegral pos))
{-# INLINE newCons #-}

newInt (Cons hdl _) n
#ifdef DEBUG
	| n < 0		= error "newInt: has to be non-negative"
#endif
	| otherwise	= Cons hdl (-(n + 1))
{-# INLINE newInt #-}

isPair (Cons _ i) = i >= 0
{-# INLINE isPair #-}

first c@(Cons hdl i)
#ifdef DEBUG
	| i < 0		= error $ "first: not a pair: " ++ show (int c)
#endif
	| otherwise	= do
		hSeek hdl AbsoluteSeek (fromIntegral i)
		n <- hReadInt hdl
		return (Cons hdl n)
{-# INLINE first #-}

second c@(Cons hdl i)
#ifdef DEBUG
	| i < 0		= error $ "second: not a pair: " ++ show (int c)
#endif
	| otherwise	= do
		hSeek hdl AbsoluteSeek (fromIntegral (i + 4))
		n <- hReadInt hdl
		return (Cons hdl n)
{-# INLINE second #-}

int c@(Cons hd i)
#ifdef DEBUG
	| i >= 0	= error $ "int: not an int: " ++ unsafePerformIO (shw c)
#endif
	| otherwise	= -(i + 1)
{-# INLINE int #-}

setFirst c@(Cons hdl i) (Cons hdl2 j)
#ifdef DEBUG
	| i < 0		= error $ "setFirst: not a pair: " ++ show (int c)
	| hdl /= hdl2	= error "setFirst: have to come from same file"
#endif
	| otherwise	= do
		hSeek hdl AbsoluteSeek (fromIntegral i)
		hWriteInt hdl j
{-# INLINE setFirst #-}

setSecond c@(Cons hdl i) (Cons hdl2 j)
#ifdef DEBUG
	| i < 0		= error $ "setSecond: not a pair: " ++ show (int c)
	| hdl /= hdl2	= error "setSecond: have to come from same file"
#endif
	| otherwise	= do
		hSeek hdl AbsoluteSeek (fromIntegral (i + 4))
		hWriteInt hdl j
{-# INLINE setSecond #-}

list [x] = newCons x (newInt x 0)
list (x:xs) = newCons x (list xs)

toList cons = if isPair cons then
		liftM2 (:) (first cons) (second cons >>= toList)
	else
		return []

nth 0 cons = first cons
nth n cons = second cons >>= nth (n - 1)

padWithZeros s = s ++ replicate (3 - length s `mod` 3) '\0'
{-# INLINE padWithZeros #-}

removeZeros s = reverse (dropWhile (=='\0') (reverse s))
{-# INLINE removeZeros #-}

pack (c1 : c2 : c3 : xs) = (shiftL c1 16 .|. shiftL c2 8 .|. c3) : pack xs
pack [] = []

unpack (n:ns) = shiftR n 16 : (shiftR n 8 .&. 255) : (n .&. 255) : unpack ns
unpack [] = []

encodeString hdl s = list $ map (newInt hdl) $ pack $ map ord $ padWithZeros s

decodeString cons = liftM (removeZeros . map chr . unpack . map int) (toList cons)
{-# INLINE decodeString #-}

shw cons = if isPair cons then
		do
		f <- first cons >>= shw
		s <- second cons >>= shw
		return $ "Cons (" ++ f ++ ") (" ++ s ++ ")"
	else
		return $ show (int cons)

cmpr s c = liftM (compare s) (decodeString c)

cmpr2 c c2 = liftM2 compare (decodeString c) (decodeString c2)

-- Dictionary operations, adapted from Data.Map

dlookup cmp k k2 t
	| isPair t	= do
		f <- first t
		val <- nth 1 t
		ord <- cmp k f
		case ord of
			LT -> do
				v2 <- nth 2 t >>= dlookup cmp k k2
				ord2 <- cmp k2 f
				case ord2 of
					LT -> return v2
					EQ -> return $ v2 ++ [val]
					GT -> liftM (\v3 -> v2 ++ val : v3) (nth 3 t >>= dlookup cmp k k2)
			EQ -> do
				ord2 <- cmp k2 f
				case ord2 of
					LT -> return []
					EQ -> return [val]
					GT -> liftM (val:) (nth 3 t >>= dlookup cmp k k2)
			GT -> nth 3 t >>= dlookup cmp k k2
	| otherwise	= return []

insert cmp kx x t = do
	referent <- first t
	if isPair referent then do	
		val <- first referent
		ord <- cmp kx val
		case ord of
			LT -> second referent >>= second >>= insert cmp kx x
			GT -> second referent >>= second >>= second >>= insert cmp kx x
			EQ -> do
				second referent >>= \s -> setFirst s x
	else
		setFirst t (list [kx, x, newInt x 0, newInt x 0])

deleteFindMin t = do
	l <- first t >>= second >>= second
	r <- first t >>= second >>= second >>= second
	fl <- first l
	fr <- first r
	if isPair fl then
			deleteFindMin l
		else do
			k <- first t >>= first
			x <- first t >>= second >>= first
			setFirst t fr
			return (k, x)

deleteFindMax t = do
	l <- first t >>= second >>= second
	r <- first t >>= second >>= second >>= second
	fl <- first l
	fr <- first r
	if isPair fr then
			deleteFindMax r
		else do
			k <- first t >>= first
			x <- first t >>= second >>= first
			setFirst t fl
			return (k, x)

delete cmpr k t = do
	referent <- first t
	when (isPair referent) $ do
	l <- second referent >>= second
	r <- second referent >>= second >>= second
	fl <- first l
	fr <- first r
	k2 <- first referent
	val <- first referent >>= second
	ord <- cmpr k k2
	case ord of
		LT -> delete cmpr k l
		GT -> delete cmpr k r
		EQ -> if isPair fl then do
				(k, x) <- deleteFindMax l
				setFirst referent k
				setFirst val x
			else if isPair fr then do
				(k, x) <- deleteFindMin r
				setFirst referent k
				setFirst val x
			else
				setFirst t (newInt t 0)

depth idx = if isPair idx then
	liftM2 (\x y -> 1 + max x y) (nth 2 idx >>= depth) (nth 3 idx >>= depth)
	else
	return 0

size idx = if isPair idx then
	liftM2 (+) (nth 2 idx >>= size) (nth 3 idx >>= size)
	else
	return 1

