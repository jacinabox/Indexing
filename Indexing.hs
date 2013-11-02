{-# LANGUAGE ScopedTypeVariables, CPP #-}

module Indexing (indexFileName, indexWrapper, fullIndex, lookKeywords, contexts, parseKeywords) where

import Data.List hiding (union, insert)
import Control.Monad
import System.Directory
import Data.Char
import Data.Word
import Data.Map (fromList, singleton, unionWith, intersectionWith, assocs, empty)
import System.IO
import Data.Function
import System.Environment
import Data.IORef
import System.FilePath
import Control.Exception
import Data.Maybe
import System.IO.Error
import Control.Parallel.Strategies
import Prelude hiding (catch)
import FileCons
import Replace
import Split
import Data.String.UTF8 (toString, fromString, toRep, fromRep)
import System.IO.Unsafe

import Unpacks
import Normalize
import Driveletters

toUTF s = map (chr . fromIntegral) (toRep $ fromString s :: [Word8])

fromUTF s = toString $ fromRep (map (fromIntegral . ord) s :: [Word8])

hGetStr _ 0 = return []
hGetStr h n = liftM2 (:) (hGetChar h) (hGetStr h (n - 1))

toUpperCase s = map toUpper s

chunks0 n ls
	| length ls < n	= []
	| otherwise	= take n ls : chunks0 n (drop n ls)

chunks n ls
	| length ls < n	= [ls]
	| otherwise	= chunks0 n ls

indexAddition text = concatMap (\chnk -> [chnk, reverse chnk]) $ chunks0 5 $ toUpperCase text ++ "   "

isPrintable c = ord c >= 32 || c == '\t' || c == '\n' || c == '\r'

indexFileName = do
	dir <- getAppUserDataDirectory "Index"
	createDirectoryIfMissing False dir
	return (dir ++ pathDelimiter : "Index.dat")

addChunkToIndex logicalName nm idx add = do
	insertSingle add (toUTF logicalName) nm idx
	insertSingle (reverse add) (toUTF logicalName) nm idx
{-# INLINE addChunkToIndex #-}

-- We maintain a distinction between "names" and "logical names," in order
-- to handle files that have been unpacked from archives. The logical names
-- are the names the user will see, and are a concatenation of paths
-- separated by @s. The other names are the places where you find the
-- temporary files that resulted from unpacking.

index name logicalName idx = catch (do
	catch (putStrLn name) (\(_ :: IOError) -> return ())
	let nm = newStr idx (toUTF logicalName)

	-- Adding the file's name to the index. We do something
	-- identical for the body of the file, but it has been
	-- optimized to not use /indexAddition/.
	mapM_
		(addChunkToIndex logicalName (newCons nm (newInt idx 0)) idx)
		(indexAddition (normalizeText (toUTF (takeFileName name))))

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
		let loop n = if n + 5 >= fromInteger sz then
				return n
			else do
				c1 <- hGetChar fl
				c2 <- hGetChar fl
				c3 <- hGetChar fl
				c4 <- hGetChar fl
				c5 <- hGetChar fl
				addChunkToIndex logicalName (newCons nm (newInt idx n)) idx (toUpperCase (normalizeText [c1, c2, c3, c4, c5]))
				loop (n + 5)
		m <- loop 0
		remaining <- hGetContents fl
		addChunkToIndex logicalName (newCons nm (newInt idx m)) idx (toUpperCase remaining ++ replicate (5 - length remaining) ' ')
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

-- The process of doing a keyword search:
--   lookKeywords deals with all the keywords the user has entered,
--   look deals with a single keyword, breaking it up into 5-letter pieces.
--   lookIdx searches for a single 5-letter piece in the index.

-- Returns all the files associated with the given key range.
lookIdxImpl :: String -> String -> Cons -> IO [(String, Int)]
lookIdxImpl k k2 idx = do
	f <- first idx
	ls <- dlookup cmpr k k2 f
	liftM concat $ mapM (\x -> toList x >>= mapM (\x -> liftM2 (,) (first x >>= str) (liftM int (second x)))) ls
{-# INLINE lookIdxImpl #-}

insertSingle k v ins idx = do
	cons <- lookupSingle cmpr k idx
	f <- first cons
	if isPair f then do
		x <- nth 1 f
		dinsert cmpr2 (newStr idx k) (newCons ins x) cons
	else
		dinsert cmpr2 (newStr idx k) (list [ins]) cons
{-# INLINE insertSingle #-}

-- A pure version of lookIdxImpl. Its use is justified by the fact that we
-- are doing queries only, so the contents of the index are unlikely to change.
lookIdx k k2 idx = unsafePerformIO (lookIdxImpl k k2 idx)

intersects ls = foldl1 intersect ls

max' f x1 x2
	| f x1 > f x2	= x1
	| otherwise	= x2

look keyword idx = concat ((do
		window <- map (\n -> max' length (drop n keyword) (reverse (take n keyword))) [0..4]
		let x:xs = do
			chunk <- chunks 5 window
			return (lookIdx chunk (chunk ++ replicate (5 - length chunk) (chr 255)) idx)
		return $ if null xs then
				x
			else let ys = intersects $ map (map fst) xs in
				filter (\(s, _) -> s `elem` ys) x)
		`using` parList (evalList rseq))
	`mplus` if length keyword == 3 then do
			c <- [' '..chr 255]
			lookIdx (c : keyword) (c : keyword ++ [chr 255]) idx
		else
			mzero

extractText name = do
	let paths = split '@' name
	finalPath <- foldM
		(\path logicalPath -> liftM (++logicalPath) (fromJust (lookup (takeExtension path) unpacks) path))
		(head paths)
		(tail paths)
	openBinaryFile finalPath ReadMode

doIntersect ls =  ls

-- First it acquires a list, /possibilities/, which is a superset of the correct
-- results. Then it filters this list by looking for the keywords at the
-- positions specified in the index.
lookKeywords keywords caseSensitive = do
	idxNm <- indexFileName
	keywords <- return $ map (normalizeText . toUTF) keywords
	idx <- openHandle idxNm
	let possibilities = assocs $ foldl1 (intersectionWith (++)) $
		map (\k -> fmap (\x -> [x]) $ foldl (unionWith (++)) empty $ map (\(x, y) -> singleton x [y]) $ look (toUpperCase k) idx)
			keywords
	let caseFunction = if caseSensitive then id else toUpperCase
	let keywords2 = map caseFunction keywords
	texts <- mapM (\(nm, ls) -> catch
			(do
				h <- extractText nm
				b <- liftM and $ mapM (\(k, ns) -> liftM ((|| isInfixOf k (caseFunction nm)) . or) $ mapM (\n -> do
						hSeek h AbsoluteSeek (toInteger ((n - 4) `max` 0))
						s <- hGetStr h (4 + length k)
						return $ isInfixOf k (caseFunction s))
						ns)
					(zip keywords2 ls)
				hSeek h AbsoluteSeek 0
				contents <- hGetContents h
				return ((nm, contents), b))
			(\(er :: IOError) -> do
				putStrLn (":::" ++ show er)
				return (undefined, False)))
		possibilities
	closeHandle idx
	return $ map fst $ filter snd texts

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
