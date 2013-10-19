{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Main (main) where

import Data.List hiding (union, insert)
import Control.Monad
import System.Directory
import Data.Char
import System.IO
import Data.Array.Unboxed (UArray)
import Data.Array.IArray hiding (index)
import Data.Function
import System.Environment
import Data.IORef
import System.Process
import Control.Concurrent.Async
import Data.List.Extras.Argmax
import System.FilePath
import Control.Exception
import Data.Maybe
import Data.Either
import Prelude hiding (catch)
import FileCons
import Replace
import Split
import System.IO.Error
import System.IO.Unsafe

import Unpacks

toUpperCase s = map toUpper s

chunks0 n ls
	| length ls < n	= [ls]
	| otherwise	= take n ls : chunks0 n (drop n ls)

chunks n ls = if length ls < n then
		[ls]
	else
		init (chunks0 n ls)

chunksPadded n ls = init cs ++ [last cs ++ replicate (n - length (last cs)) ' ']
	where cs = chunks0 n ls

indexAddition text = concatMap (\chnk -> [chnk, reverse chnk]) $ chunksPadded 5 $ toUpperCase text

isPrintable c = c `elem` "\t\n\r" || ord c >= 32

indexFileName n = do
	dir <- getAppUserDataDirectory "Index"
	createDirectoryIfMissing False dir
	return (dir ++ pathDelimiter : "Index" ++ show n ++ ".dat")

openIndexes mode = mapM (\x -> do
		idxNm <- indexFileName x
		openHandle idxNm mode)
	[1..8]

flatten hdl [] = newInt hdl 0
flatten hdl (x:_) = x

addChunkToIndex logicalName nm idx add = do
	(ls, existing) <- lookIdxImpl add add idx
	when (logicalName `notElem` ls) $
		insert cmpr2 (encodeString idx add) (newCons nm existing) idx
	(ls, existing) <- lookIdxImpl (reverse add) (reverse add) idx
	when (logicalName `notElem` ls) $
		insert cmpr2 (encodeString idx (reverse add)) (newCons nm existing) idx
{-# INLINE addChunkToIndex #-}

-- We maintain a distinction between "names" and "logical names," in order
-- to handle files that have been unpacked from archives. The logical names
-- are the names the user will see, and are a concatenation of paths
-- separated by @s. The other names are the places where you find the
-- temporary files that resulted from unpacking.

index name logicalName idx = catch (do
	let nm = encodeString idx logicalName

	-- Adding the file's name to the index. Something identical is done
	-- for the body of the file, but it has been optimized to not use
	-- /indexAddition/.
	mapM_
		(addChunkToIndex logicalName nm idx)
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
			addChunkToIndex logicalName nm idx (toUpperCase [c1, c2, c3, c4, c5])
			loop (n - 5)
		loop (fromInteger sz)
		remaining <- hGetContents fl
		addChunkToIndex logicalName nm idx (toUpperCase remaining ++ replicate (5 - length remaining) ' ')
		hClose fl)
	(\(er :: IOError) -> putStrLn (":::" ++ show er))

doUnpacks dir logicalDir fls dirs = liftM (map (\nm -> Right (dir ++ nm, logicalDir ++ nm)) dirs ++) $
	mapM (\nm -> let def = Left (dir ++ nm, logicalDir ++ nm) in
		maybe
		(return def)
		(\f -> catch (do
			unpacked <- f (dir ++ nm)
			return (Right (unpacked, logicalDir ++ nm ++ "@")))
			(\(er :: IOError) -> do
				putStrLn (":::" ++ show er)
				return def))
		(lookup (takeExtension nm) unpacks))
	fls

details name code = do
	userdata <- getAppUserDataDirectory "Index"
	tmp <- getTemporaryDirectory
	unless
		(name == userdata || name == tmp || name == "/dev" || name == "/sys")
		code

mapPair f (x, y) = (f x, f y)

indexDirectory dir logicalDir = catch (do
	idxs <- openIndexes ReadWriteMode

	-- Splits the directory's contents into files and directories
	contents <- getDirectoryContents dir
	isFl <- mapM (doesFileExist . (dir++)) contents
	let (fls, dirs) = mapPair (map snd) $ partition fst $ zip isFl contents

	-- Unpacks the files that can be unpacked, turning them into directories
	-- for indexing. This results in new lists of files and directories.
	paths <- doUnpacks dir logicalDir fls dirs
	let (fls, dirs) = partitionEithers paths

	-- Indexes the files.
	mapM_ (\chnk -> do
		mapM_ (putStrLn . snd) chnk
		mapConcurrently
			(\((path, logicalPath), idx) -> index path logicalPath idx)
			(zip chnk idxs))
		(chunks0 8 fls)

	mapM_ closeHandle idxs

	-- Indexes the directories separately.
	mapM_ (\(path, logicalPath) ->
		unless (takeFileName path == "." || takeFileName path == "..") $
			details path {-Primary control flow:-}(indexDirectory (path ++ [pathDelimiter]) (logicalPath ++ [pathDelimiter])))
		dirs)
	(\(er :: IOError) -> putStrLn (":::" ++ show er))

#ifdef WIN32
fullIndex = do
	letters <- readProcess "driveletters.exe" [] ""
	mapM_ (\dir -> indexDirectory dir dir) (lines letters)
#else
fullIndex = indexDirectory "/" "/"
#endif

intersects ls = foldl1 intersect ls

-- The process of doing a keyword search:
--   lookKeywords deals with all the keywords the user has entered,
--   look deals with a single keyword, breaking it up into 5-letter pieces.
--   lookIdx searches for a single 5-letter piece in the index.

-- Returns a pair, with the first element being all the files associated with
-- the given key range, and the second being the raw Cons associated with
-- k, for adding onto.
lookIdxImpl k k2 idx = do
	f <- first idx
	ls <- dlookup cmpr k k2 f
	converted <- liftM concat $ mapM (\x -> toList x >>= mapM decodeString) ls
	return (converted, flatten idx ls)
{-# INLINE lookIdxImpl #-}

-- A pure version of lookIdxImpl. Its use is justified by the fact that we
-- are doing queries only, so the contents of the index are unlikely to change.
lookIdx k k2 idxs = concat $ map fst $ unsafePerformIO (mapConcurrently (lookIdxImpl k k2) idxs)

look keyword idx = do
		window <- map (\n -> argmax length [drop n keyword, reverse (take n keyword)]) [0..4]
		intersects [ lookIdx chunk (chunk ++ replicate (5 - length chunk) '~') idx | chunk <- chunks 5 window ]
	`mplus` if length keyword == 3 then
			[ res | chr <- [' '..'~'], res <- lookIdx (chr : keyword) (chr : keyword ++ "~") idx ]
		else
			[]

extractText name = do
	let paths = split '@' name
	finalPath <- foldM
		(\path logicalPath -> liftM (++logicalPath) (fromJust (lookup (takeExtension path) unpacks) path))
		(head paths)
		(tail paths)
	readFile finalPath

-- First it acquires a list, /possibilities/, which is a superset of the correct
-- results. Then it winnows this list down by searching for the keywords
-- in the texts of the files.
lookKeywords keywords caseSensitive = do
	let longKeywords = filter ((>=5) . length) keywords
	idxs <- openIndexes ReadMode
	let possibilities = nub $ intersects $ map ((`look` idxs) . toUpperCase)
		$ if null longKeywords then keywords else longKeywords
	texts <- mapM (\nm -> liftM (\str -> (nm, str)) (extractText nm))
		possibilities
	mapM_ closeHandle idxs
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

contexts keywords text caseSensitive = concatMap (\k -> case findIndex (\suffix -> isPrefixOf (caseFunction k) suffix) (tails (caseFunction text)) of
		Just i -> let context = take 67 (drop (i - 33) text) in
			if all isPrintable context then
				pad 5 (show $ lineNumber i 1 $ lines text) ++ " ..." ++ replace [("\n", " "), ("\t", " ")] context ++ "...\n"
			else
				""
		Nothing -> "")
	keywords where
	caseFunction = if caseSensitive then id else toUpperCase

main = do
	args <- getArgs
	let keywords = filter ((>2) . length) args
	if not (null args) && head args == "-i" then
		if length args == 1 then
			fullIndex
		else do
			dir <- canonicalizePath (args !! 1)
			indexDirectory dir dir
	else if null keywords then do
		putStrLn "Index: no keywords"
		putStrLn "Use -c for case sensitive search"
		putStrLn "Use -i to do a full index"
		putStrLn "Use -i dir to index a directory"
	else do
		let caseSensitive = "-c" `elem` args
		results <- lookKeywords keywords caseSensitive
		mapM_ (\(nm, text) -> do
				putStrLn ""
				putStrLn ("  " ++ nm)
				putStr (contexts keywords text caseSensitive))
			results
		when (null results) (putStrLn "Index: no results found")
