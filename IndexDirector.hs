{-# LANGUAGE ScopedTypeVariables, CPP #-}

module IndexDirector (indexDatabase, indexWrapper, fullIndex, lookKeywords, getIndex, parseKeywords) where

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
import Data.ByteString.Char8 (unpack, hGet)
import Prelude hiding (catch)

import Replace
import Unpacks
import Normalize
import Driveletters
import Indexing

toUpperCase s = map toUpper s

indexFileName = do
	dir <- getAppUserDataDirectory "Indexing"
	createDirectoryIfMissing False dir
	return (dir ++ pathDelimiter : "Index.dat", dir ++ pathDelimiter : "Overflow.dat")

-- I maintain a distinction between "names" and "logical names," in order
-- to handle files that have been unpacked from archives. The logical names
-- are the names the user will see, and are a concatenation of paths
-- separated by @s. The other names are the places where you find the
-- temporary files that resulted from unpacking.

details1 name logicalName idx code = putStrLn logicalName >> maybe
	code
	(\f -> try (f name) >>= either
		(\(ex :: IOError) -> putStr "*** " >> print ex)
		(\unpacked -> indexDirectory unpacked (logicalName ++ "@") idx))
	(lookup (takeExtension name) unpacks)

details2 name code = do
	userdata <- getAppUserDataDirectory "Indexing"
	tmp <- getTemporaryDirectory
	unless
		(name == appendDelimiter userdata || name == appendDelimiter tmp || name == "/dev" || name == "/sys")
		code

details3 code = catch (catch code 
	(\(ex :: IndexingError) -> putStr "*** " >> print ex))
	(\(ex :: IOError) -> putStr "*** " >> print ex)

-- When a file is an unpackable archive, I do the unpacking, then start
-- indexing the resulting temporary directory.
indexDirectory dir logicalDir idx = details3 $ do
	contents <- getDirectoryContents dir
	mapM_ (\nm -> let
			name = dir ++ nm
			logicalName = logicalDir ++ nm in
		unless (nm == "." || nm == "..") $ do
		b <- doesFileExist name
		if b then
				details1 name logicalName idx {-Primary control flow:-}(bracket (openBinaryFile name ReadMode)
			hClose
			$ \hdl -> do
			contents <- hGetContents hdl
			catch (index idx logicalName contents) (\(ex :: IndexingError) -> putStr "*** " >> print ex))
			else
				details2 name {-Primary control flow:-}(indexDirectory (name ++ [pathDelimiter]) (logicalName ++ [pathDelimiter]) idx))
		contents

indexDatabase ident idx = undefined{-do
	tables <- database ident
	mapM_ (\tab -> do
		recs <- getTable ident tab
		mapM_ (\rec -> do
			Rec ls <- record ident tab rec
			index idx (appendDelimiter ident ++ appendDelimiter tab ++ rec) $ concatMap (maybe "@" ('@':) . snd) ls)
			recs)
		tables-}

indexWrapper dir = do
	(idxNm, idxNm2) <- indexFileName
	idx <- openIndex idxNm idxNm2
	finally
		(indexDirectory dir dir idx)
		(closeIndex idx)

#ifdef WIN32
fullIndex = do
	letters <- driveLetters
	mapM_ indexWrapper letters
#else
fullIndex = indexWrapper "/"
#endif

intersection ((x, y):xs) ls = if null with then
		intersection xs ls
	else
		(x, y ++ concatMap snd with) : intersection xs without where
	(with, without) = partition ((==x) . fst) ls
intersection [] _ = []

intersects ls = foldl1 intersection (map (map (\(x, y) -> (x, [y]))) ls)

-- The process of doing a keyword search:
--   lookKeywords deals with all the keywords the user has entered,
--   lookUp deals with a single keyword.

-- First it acquires a list, /possibilities/, which is a superset of the correct
-- results. Then it winnows this list down by searching for the keywords
-- in the texts of the files.
lookKeywords idx keywords options = do
	keywords <- return $ map normalizeText keywords
	results <- liftM intersects $ mapM (lookUp idx options) keywords
	mapM (contexts keywords) results

getIndex = do
	(idxNm, idxNm2) <- indexFileName
	idx <- openIndex idxNm idxNm2
	readIndex idx

-- This function produces the contexts for a search.
contexts keywords (name, locs) = liftM ((,) name) $ mapM (\(k, loc) -> do
	hdl <- openBinaryFile name ReadMode
	sz <- hFileSize hdl
	hSeek hdl AbsoluteSeek $ toInteger $ (loc - 33) `max` 0
	bs <- hGet hdl $ 67 `min` (fromInteger sz - (fromIntegral loc - 33))
	return $ "..." ++ replace [("\n", " "), ("\t", " "), ("\r", " ")] (unpack bs) ++ "...")
	$ zip keywords locs

parseKeywords (c:cs)
	| c == '"'	= let (kw, rest) = break (=='"') cs in
		kw : parseKeywords (drop 2 rest)
	| otherwise	= let (kw, rest) = break (==' ') (c:cs) in
		kw : parseKeywords (drop 1 rest)
parseKeywords [] = []
