{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TypeOperators, CPP #-}

-- | Monotone hash based index.
module Indexing (pad, openIndex, closeIndex, IndexRepr, Index, IndexingError(..), QueryOptions(..), index, lookUp) where

import System.Random
import Control.Monad.Random
import System.IO
import Control.Exception
import Control.Monad.Loops
import Control.Monad
import Control.Parallel.Strategies
import Control.Concurrent
import Control.Arrow
import Generics.Pointless.Functors
import Data.Array.IArray hiding (index)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List hiding (nub)
import Data.Typeable
import Data.Function
import Data.ByteString.Char8 (ByteString, unpack, hGet)
import Foreign.Storable
import Foreign.Marshal.Utils
import qualified Data.Map as M
import Prelude hiding (catch)

import System.IO.Unsafe

import File.Mapped
import File.Graph
import LazySequence
import Unpacks

openIndex :: FilePath -> IO Index
openIndex path = do
	ptr <- openMapped path
	p2 <- peekPtr (fsecond ptr)
	when (offset p2 == 0) $ do
		closeMapped (fileSrc ptr)
		writeGraph path (replicate 256 [] :: IndexRepr) M.empty
	checkedOpen path

closeIndex :: Index -> IO ()
closeIndex = closeMapped . fileSrc

pad _ 0 ls = ls
pad with n [] = replicate n with
pad with n (x:xs) = x : pad with (n - 1) xs

windows0 sz step ls = take sz ls : if null dr then [] else windows0 sz step dr where
	dr = drop step ls

windows sz step = map (pad '\0' sz) . windows0 sz step

bits :: Array Int [Int]
bits = listArray (0, 255) $ [] : evalRand (mapM (\_ -> mapM (\_ -> getRandomR (0, 15)) [(), ()]) [1..255]) (mkStdGen 0)

hashByte f hash x = if x == '\0' || ord x > 255 then
		hash
	else
		foldl f hash $ bits ! ord x

hashWindow :: Word16 -> String -> Word16
hashWindow init wnd = foldl (hashByte setBit) init wnd

compatiblePlaces :: String -> [Word16]
compatiblePlaces window = map (foldl setBit hash) $ choose bits where
	hash = hashWindow 0 window
	bits = filter (not . testBit hash) [0..15]

choose [] = [[]]
choose (x:xs) = map (x:) chs ++ chs where chs = choose xs

data IndexingError = PathTooLong -- a path was too long
	| ResourceError deriving (Show, Typeable) -- an internal error occurred

instance Exception IndexingError

data QueryOptions = QueryOptions { caseSensitive :: !Bool, inArchives :: !Bool, wordsOnly :: !Bool }

{-# NOINLINE buffer #-}
buffer = unsafePerformIO (new 0)

hGetInt32 :: Handle -> IO Int32
hGetInt32 hdl = do { hGetBuf hdl buffer 4; peek buffer }

hPutInt32 :: Handle -> Int32 -> IO ()
hPutInt32 hdl x = do { poke buffer x; hPutBuf hdl buffer 4 }

printable x = ord x >= 32 && ord x <= 255 || ord x == 10 || ord x == 13

interesting = any (`notElem` "\t\n\r ")
{-
packInt :: [Int32] -> Int32
packInt [x1,x2,x3,x4] = shiftL x1 24 .|. shiftL x2 16 .|. shiftL x3 8 .|. x4

-- Squeezes the important info in a string into 16 bits
packInt1 :: [Int32] -> Int32
packInt1 [x1,x2,x3,x4] = shiftL x1 12 .|. shiftL x2 8 .|. shiftL x3 4 .|. x4

unpackInt :: Int32 -> [Int32]
unpackInt i = [shiftR i 24, shiftR i 16 .&. 255, shiftR i 8 .&. 255, i .&. 255]
-}

type L t = Fix (Const () :+: (Const t :*: Id))

lToList :: L t -> [t]
lToList (Inn ei) = either
	(const [])
	(\(x, xs) -> x : lToList xs)
	ei

type IndexRepr = [[L (Int, FilePath)]]

type Index = FilePtr IndexRepr

index :: Index -> FilePath -> String -> IO ()
index idx path contents = do
	-- Check path length
	when (length path > 128) $ throwIO PathTooLong

	-- Add the path to the path array
	p <- malloc (fileSrc idx) (size path)
	writeGraphAt p path M.empty
	pathPtr <- peekPtr (toRepr p)

	let contents' =  map toUpper $ takeWhile printable contents

	-- Index the file
	foldr (\(i, window) next -> when (interesting window) $ do
		-- Index the word sequence
		let n = hashWindow 0 window
		let n1 = n .&. 255
		let n2 = n `shiftR` 8
		p <- elementAt idx (fromIntegral n1)
		sz <- peekPtr (toRepr p) >>= fpeek . ffirst
		when (sz == 0) $ writeGraphAt p (replicate 256 (Inn (Left ()))) M.empty
		p2 <- elementAt p (fromIntegral n2)
		(fls, _) <- readGraphAt p2
		unless ((i, path) `elem` lToList fls) $ do -- Save the path
			ptr <- peekPtr (toRepr p2)
			let x = Right ((fromIntegral i, toRepr pathPtr), ptr)
			p3 <- malloc (fileSrc idx) (size (Left () :: Either () ((Int, FilePath), L (Int, FilePath))))
			fpoke p3 x
			pokePtr (toRepr p2) p3
		next)
		(return ())
		(zip [0,5..] (windows 10 5 contents'))
		-- ++ map ((+1) *** take 10 . tail) (filter (\(_, x:_) -> x `elem` " \t\r\n") $ zip [0..] $ init $ tails contents'))

getFile' options path = if inArchives options || '@' `notElem` path then
		liftM Just $ getFile path
	else
		return Nothing

bigZip ls = if any null ls then [] else map head ls : bigZip (map tail ls)

prnt x = unsafePerformIO (putStrLn x) `seq` x

data Key t u = Key t u

unKey (Key x y) = (x, y)

instance (Eq t) => Eq (Key t u) where
	(==) = (==) `on` \(Key x _) -> x

instance (Ord t) => Ord (Key t u) where
	compare = compare `on` \(Key x _) -> x

nub ls = catMaybes $ map snd $ scanl (\(st, _) x -> if M.member x st then
		(st, Nothing)
	else
		(M.insert x () st, Just x))
	(M.empty, Nothing)
	ls

swap (x, y) = (y, x)

lookUp :: Index -> QueryOptions -> String -> IO [(FilePath, (FilePath, Int))]
lookUp idx options string = do
	let string1 = (if caseSensitive options then id else map toUpper) string
	let wns = concatMap (\(i, s) -> map ((,) i) $ compatiblePlaces $ take 10 $ pad '\0' 10 s) $ zip [0..4] $ tails $ map toUpper string
	locations <- lazyMapM
		((`using` evalList rseq) . map (swap . unKey) . nub . map (uncurry Key . swap) . concat)
		(\(j, hash) -> do
			let n1 = fromIntegral hash .&. 255
			let n2 = fromIntegral hash `shiftR` 8
			p <- elementAt idx n1
			sz <- peekPtr (toRepr p) >>= fpeek . ffirst
			if sz == 0 then
				return []
				else do
				p2 <- elementAt p n2
				(res, _) <- readGraphAt p2
				return (lToList res))
		wns
	lazyMapM
		((`using` evalList rseq) . map unKey . nub . map (uncurry Key) . concat)
		(\(i, path) -> do
		path' <- getFile' options path
		maybe
			(return [])
			(\path' -> do
			catch
				(do
				inxd <- openBinaryFile path' ReadMode
				finally (do
					hSeek inxd AbsoluteSeek $ toInteger $ 0 `max` i
					s <- hGet inxd (length string1)
					return $ if string1 == (if caseSensitive options then id else map toUpper) (unpack s) then
							[(path, (path', i))]
						else
							[])
					(hClose inxd))
				(\(ex :: IOError) -> putStr "*** " >> print ex >> return []))
			path')
		locations
