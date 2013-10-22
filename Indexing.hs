{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Indexing (indexFileName, indexWrapper, fullIndex, lookKeywords, contexts, parseKeywords) where

import Data.List hiding (insert, lookup, union)
import qualified Data.List
import Control.Monad
import System.Directory
import Data.Char
import System.IO
import Data.Array.Unboxed (UArray)
import Data.Array.IArray hiding (index, assocs)
import Data.Function
import System.Environment
import Data.IORef
import System.Process
import System.FilePath
import Control.Exception
import Data.Maybe
import Data.Map (insert, lookup, union, empty, assocs)
import qualified Data.Map as Map
import Prelude hiding (catch, lookup)
import Replace
import FileCons
import qualified Split
import System.IO.Error
import System.IO.Unsafe

import Unpacks

toUpperCase s = map toUpper s

chunks0 n ls
	| length ls < n	= []
	| otherwise	= take n ls : chunks0 n (drop n ls)

chunks n ls = if length ls < n then
		[ls]
	else
		chunks0 n ls

indexAddition text = concatMap (\chnk -> [chnk, reverse chnk]) $ chunks0 5 $ toUpperCase text ++ "   "

isPrintable c = ord c >= 32 || c == '\t' || c == '\n' || c == '\r'

indexFileName = do
	dir <- getAppUserDataDirectory "Index"
	createDirectoryIfMissing False dir
	return (dir ++ pathDelimiter : "Index.dat")

addChunkToIndex logicalName idx add = do
	mp <- readIORef idx
	let ls = maybe [] id (lookup add mp)
	when (logicalName `notElem` ls) $
		writeIORef idx $! insert add (logicalName : ls) mp
	mp <- readIORef idx
	let ls = maybe [] id (lookup (reverse add) mp)
	when (logicalName `notElem` ls) $
		writeIORef idx $! insert (reverse add) (logicalName : ls) mp
{-# INLINE addChunkToIndex #-}

{-dlookup k k2 mp = maybeToList (lookup k mp) ++ map snd (takeWhile ((<=k2) . fst) (assocs r))
	where (_, r) = split k mp-}

-- We maintain a distinction between "names" and "logical names," in order
-- to handle files that have been unpacked from archives. The logical names
-- are the names the user will see, and are a concatenation of paths
-- separated by @s. The other names are the places where you find the
-- temporary files that resulted from unpacking.

index name logicalName idx = catch (do
	putStrLn name

	-- Adding the file's name to the index.
	mapM_
		(addChunkToIndex logicalName idx)
		(indexAddition (takeFileName name))

	-- Checks to see if the file is binary. If so, we skip it.
	fl <- openBinaryFile name ReadMode
	let
		loop 0 = return True
		loop n = do
			b <- hIsEOF fl
			if b then
				return True
			else do
				c <- hGetChar fl
				if isPrintable c then
					loop (n - 1)
				else
					return False
	printable <- loop 500
	hClose fl

	when printable $ do
		-- Breaks the file up into 5-letter chunks and associates these chunks
		-- with the given file.
		fl <- openBinaryFile name ReadMode
		sz <- hFileSize fl
		let loop n = when (n >= 5) $ do
			c1 <- hGetChar fl
			c2 <- hGetChar fl
			c3 <- hGetChar fl
			c4 <- hGetChar fl
			c5 <- hGetChar fl
			addChunkToIndex logicalName idx (toUpperCase [c1, c2, c3, c4, c5])
			loop (n - 5)
		loop (fromInteger sz)
		remaining <- hGetContents fl
		addChunkToIndex logicalName idx (toUpperCase remaining ++ replicate (5 - length remaining) ' ')
		hClose fl)
	(\(er :: IOError) -> putStrLn (":::" ++ show er))

details1 name logicalName idx code = catch (maybe
	code
	(\f -> do
		unpacked <- f name
		indexDirectory unpacked (logicalName ++ "@") idx)
	(Data.List.lookup (takeExtension name) unpacks))
	(\(er :: IOError) -> putStrLn $ ":::" ++ show er)

details2 name code = do
	userdata <- getAppUserDataDirectory "Index"
	tmp <- getTemporaryDirectory
	unless
		(name == userdata || name == tmp || name == "/dev" || name == "/sys")
		code

-- What this does is decide, for each thing in a directory,
-- whether it is a file or a directory. If it is a file, we call the
-- /index/ function to index it. If it is a directory, we recurse in order to
-- index the directory. (There is a special case when a file is an
-- unpackable archive, in which case we do the unpacking, then start
-- indexing the resulting /temporary directory/.)
indexDirectory dir logicalDir idx = catch (do
	contents <- getDirectoryContents dir
	mapM_ (\nm -> let name = dir ++ nm in
		let logicalName = logicalDir ++ nm in
		unless (nm == "." || nm == "..") $ do
		b <- doesFileExist name
		if b then
				details1 name logicalName idx {-Primary control flow:-}(index name logicalName idx)
			else
				details2 name {-Primary control flow:-}(indexDirectory (name ++ [pathDelimiter]) (logicalName ++ [pathDelimiter]) idx))
		contents)
	(\(er :: IOError) -> putStrLn (":::" ++ show er))

readDict dict idx
	| isPair idx	= do
		k <- nth 0 idx >>= decodeString
		v <- nth 1 idx >>= toList >>= mapM (\x -> do
			-- This maintains a dictionary of strings to reduce duplication. When
			-- a Cons shows up in the dictionary, we use the string that was
			-- originally saved there.
			mp <- readIORef dict
			case lookup (getPtr x) mp of
				Just s -> return s
				Nothing -> do
					s <- decodeString x
					writeIORef dict $! insert (getPtr x) s mp
					return s)
		m1 <- nth 2 idx >>= readDict dict
		m2 <- nth 3 idx >>= readDict dict
		return $! insert k v (union m1 m2)
	| otherwise	= return empty

writeDict idx [] = newInt idx 0
writeDict idx ls = list [encodeString idx k, list v, writeDict idx fs, writeDict idx sn]
	where
		fs = take (length ls `div` 2) ls
		(k, v):sn = drop (length ls `div` 2) ls

indexWrapper dir = do
	-- Reads the listing from the index.
	idxNm <- indexFileName
	idxVal <- catch (do
		idx <- openHandle idxNm ReadMode
		f <- first idx
		dict <- newIORef empty
		idxVal <- readDict dict f
		closeHandle idx
		return idxVal)
		(\(_ :: IOError) -> return empty)

	-- Indexes the directory.
	idx <- newIORef idxVal
	indexDirectory dir dir idx
	idxVal <- readIORef idx

	-- Again, reduces duplication by keeping a dictionary of strings.
	idx <- openHandle idxNm WriteMode
	names <- newIORef empty
	ascs <- mapM
		(\(k, ls) -> liftM ((,) k) $ mapM
			(\x -> do
				namesVal <- readIORef names
				case lookup x namesVal of
					Just cons -> return cons
					Nothing -> do
						let y = encodeString idx x
						writeIORef names $! insert x y namesVal
						return y)
			ls)
		(assocs idxVal)

	-- Saves the index.
	setFirst idx (writeDict idx ascs)
	closeHandle idx

#ifdef WIN32
fullIndex = do
	letters <- readProcess "driveletters.exe" [] ""
	mapM_ indexWrapper (lines letters)
#else
fullIndex = indexWrapper "/"
#endif

intersects ls = foldl1 intersect ls

-- The process of doing a keyword search:
--   lookKeywords deals with all the keywords the user has entered,
--   look deals with a single keyword, breaking it up into 5-letter pieces.
--   lookIdx searches for a single 5-letter piece in the index.

max' f x1 x2
	| f x1 > f x2	= x1
	| otherwise	= x2

-- The use of unsafePerformIO is justified by the fact that we are
-- only doing reads and the index is unlikely to change.
lookIdx k k2 idx = concat $ unsafePerformIO $ first idx >>= dlookup cmpr k k2 >>= mapM (\x -> toList x >>= mapM decodeString)

look keyword idx = do
		window <- map (\n -> max' length (drop n keyword) (reverse (take n keyword))) [0..4]
		intersects [ lookIdx chunk (chunk ++ replicate (5 - length chunk) '~') idx | chunk <- chunks 5 window ]
	`mplus` if length keyword == 3 then
			[ res | chr <- [' '..'~'], res <- lookIdx (chr : keyword) (chr : keyword ++ "~") idx ]
		else
			[]

extractText name = do
	let paths = Split.split '@' name
	finalPath <- foldM
		(\path logicalPath -> liftM (++logicalPath) (fromJust (Data.List.lookup (takeExtension path) unpacks) path))
		(head paths)
		(tail paths)
	readFile finalPath

-- First it acquires a list, /possibilities/, which is a superset of the correct
-- results. Then it winnows this list down by searching for the keywords
-- in the texts of the files.
lookKeywords keywords caseSensitive = do
	idxNm <- indexFileName
	idx <- openHandle idxNm ReadMode
	let longKeywords = filter ((>=5) . length) keywords
	let possibilities = nub $ intersects $ map ((`look` idx) . toUpperCase)
		$ if null longKeywords then keywords else longKeywords
	texts <- mapM (\nm -> liftM (\str -> (nm, str)) (extractText nm))
		possibilities
	closeHandle idx
	let caseFunction = if caseSensitive then id else toUpperCase
	let keywords2 = map caseFunction keywords
	return $ filter
		(\(nm, str) -> all (\k -> any (isInfixOf k . caseFunction) [takeFileName nm, str]) keywords2)
		texts

lineNumber idx num (line:lines) = if idx < length line then
		num
	else
		lineNumber (idx - length line - 1) (num + 1) lines

pad n s = replicate (n - length s) ' ' ++ s

contexts keywords text caseSensitive = catMaybes $ map (\k -> case findIndex (\suffix -> isPrefixOf (caseFunction k) suffix) (tails (caseFunction text)) of
		Just i -> let context = take 67 (drop (i - 33) text) in
			if all isPrintable context then
				Just $ pad 5 (show $ lineNumber i 1 $ lines text) ++ " ..." ++ replace [("\n", " "), ("\t", " ")] context ++ "..."
			else
				Nothing
		Nothing -> Nothing)
	keywords where
	caseFunction = if caseSensitive then id else toUpperCase

parseKeywords (c:cs)
	| c == '"'	= let (kw, rest) = break (=='"') cs in
		kw : parseKeywords (drop 2 rest)
	| otherwise	= let (kw, rest) = break (==' ') (c:cs) in
		kw : parseKeywords (drop 1 rest)
parseKeywords [] = []
