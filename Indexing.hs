{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Indexing (indexFileName, indexWrapper, fullIndex, lookKeywords, contexts, parseKeywords) where

import Data.List hiding (union, insert)
import Control.Monad
import System.Directory
import System.IO
import Data.Char
import Data.Function
import System.Environment
import Data.IORef
import System.FilePath
import Control.Exception
import Data.Maybe
import System.IO.Error
import Prelude hiding (catch)
import FileCons
import Replace
import Split

import Unpacks
import Normalize
import Driveletters

toUpperCase s = map toUpper s

chunks0 n ls
	| length ls < n	= []
	| otherwise	= take n ls : chunks0 n (drop n ls)

chunks n ls
	| length ls < n	= [ls]
	| otherwise	= chunks0 n ls

windows n ls = map (\ls2 -> ls2 ++ replicate (n - length ls2) ' ') $ filter ((>2) . length) $ map (take n) $ tails ls

indexAddition text = windows 5 $ toUpperCase $ normalizeText text

isPrintable c = ord c >= 32 || c == '\t' || c == '\n' || c == '\r'

indexFileName = do
	dir <- getAppUserDataDirectory "Index"
	createDirectoryIfMissing False dir
	return (dir ++ pathDelimiter : "Index.dat")

-- We maintain a distinction between "names" and "logical names," in order
-- to handle files that have been unpacked from archives. The logical names
-- are the names the user will see, and are a concatenation of paths
-- separated by @s. The other names are the places where you find the
-- temporary files that resulted from unpacking.

index name logicalName idx = catch (do
	catch (putStrLn name) (\(_ :: IOError) -> return ())
	let nm = newStr idx logicalName

	-- Adding the file's name to the index. Something identical
	-- is done for the body of the file, but it has been
	-- optimized to not use /indexAddition/.
	mapM_
		(\add -> insertSingle add logicalName nm idx)
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
		-- Associates each suffix array with the given file.
		fl <- openFile name ReadMode
		hSetEncoding fl utf8
		c1 <- hGetChar fl
		c2 <- hGetChar fl
		c3 <- hGetChar fl
		c4 <- hGetChar fl
		c5 <- hGetChar fl
		let s = toUpperCase (normalizeText [c1, c2, c3, c4, c5])
		insertSingle s logicalName nm idx
		let loop s = do
			b <- hIsEOF fl
			if b then do
					insertSingle (tail s ++ " ") logicalName nm idx
					insertSingle (drop 2 s ++ "  ") logicalName nm idx
				else do
					c <- hGetChar fl
					let s2 = tail s ++ toUpperCase (normalizeText [c])
					insertSingle s2 logicalName nm idx
					loop s2
		loop s
		hClose fl)
	(\(er :: IOError) -> putStrLn (":::" ++ show er))

details1 name logicalName idx code = catch
	(maybe
	code
	(\f -> do
		unpacked <- f name
		indexDirectory unpacked (logicalName ++ "@") idx)
	(lookup (takeExtension name) unpacks))
	(\(er :: IOError) -> putStrLn (":::" ++ show er))

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

indexWrapper dir = do
	idxNm <- indexFileName
	idx <- openHandle idxNm
	indexDirectory dir dir idx
	closeHandle idx

#ifdef WIN32
fullIndex = do
	letters <- driveLetters
	mapM_ indexWrapper letters
#else
fullIndex = indexWrapper "/"
#endif

intersects ls = foldl1 intersect ls

-- The process of doing a keyword search:
--   lookKeywords deals with all the keywords the user has entered,
--   look deals with a single keyword, breaking it up into 5-letter pieces.
--   lookIdx searches for a single 5-letter piece in the index.

-- Returns all the files associated with the given key range.
lookIdx k k2 idx = do
	f <- first idx
	ls <- dlookup cmpr k k2 f
	liftM concat $ mapM (\x -> toList x >>= mapM str) ls
{-# INLINE lookIdx #-}

insertSingle k v ins idx = do
	cons <- lookupSingle cmpr k idx
	f <- first cons
	if isPair f then do
		x <- nth 1 f
		dinsert cmpr2 (newStr idx k) (newCons ins x) cons
	else
		dinsert cmpr2 (newStr idx k) (list [ins]) cons
{-# INLINE insertSingle #-}

look keyword idx = liftM intersects $ mapM (\chunk -> lookIdx chunk (chunk ++ replicate (5 - length chunk) (chr 32767)) idx) (chunks 5 keyword)

extractText name = do
	let paths = split '@' name
	finalPath <- foldM
		(\path logicalPath -> liftM (++logicalPath) (fromJust (lookup (takeExtension path) unpacks) path))
		(head paths)
		(tail paths)
	fl <- openFile finalPath ReadMode
	hSetEncoding fl utf8
	hGetContents fl

details3 code = catch
	(return $! code)
	(\(er :: IOError) -> do
		putStrLn (":::" ++ show er)
		return False)

-- First it acquires a list, /possibilities/, which is a superset of the correct
-- results. Then it winnows this list down by searching for the keywords
-- in the texts of the files.
lookKeywords keywords caseSensitive = do
	idxNm <- indexFileName
	keywords <- return $ map normalizeText keywords
	idx <- openHandle idxNm
	possibilities <- liftM (nub . intersects) (mapM ((`look` idx) . toUpperCase) keywords)
	texts <- mapM (\nm -> liftM (\str -> (nm, str)) (catch
			(extractText nm)
			(\(er :: IOError) -> do
				putStrLn (":::" ++ show er)
				return "")))
		possibilities
	closeHandle idx
	let caseFunction = if caseSensitive then id else toUpperCase
	let keywords2 = map caseFunction keywords
	filterM
		(\(nm, str) -> details3 $ all (\k -> any (isInfixOf k . caseFunction . normalizeText) [takeFileName nm, str]) keywords2)
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
