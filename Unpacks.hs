{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Unpacks (unpacks, pathDelimiter) where

import Data.List
import System.Directory
import System.Process
import System.FilePath
import Control.Exception
import Prelude hiding (catch)
import Replace

#ifdef WIN32
pathDelimiter = '\\'
#else
pathDelimiter = '/'
#endif

convertHtml0 ('<':xs) = convertHtml0 $ tail $ snd $ break (=='>') xs
convertHtml0 (x:xs) = x : convertHtml0 xs
convertHtml0 [] = []

convertHtml s = replace [("&nbsp;", " "), ("&lt;", "<"), ("&gt;", ">"), ("&amp;", "&"), ("&quot;", "\"")]
	$ convertHtml0 s

getUnpackDir n = do
	tmp <- getTemporaryDirectory
	let path = tmp ++ show n ++ [pathDelimiter]
	catch
		(do
			createDirectory path
			return path)
		(\(_ :: IOError) -> getUnpackDir (n + 1))

convertFile f name = do
	path <- getUnpackDir 0
	txt <- readFile name
	writeFile (path ++ "converted.dat") (f txt)
	return path 

unpack cmd switches name = do
	path <- getUnpackDir 0
	let path2 = path ++ takeFileName name
	copyFile name path2
	readProcess cmd (switches ++ [path2]) ""
	catch (removeFile path2) (\(_ :: IOError) -> return ())
	return path

-- A table of conversion functions, that take their filenames
-- as strings and unpack them into temporary directories.
unpacks = [(".htm", convertFile convertHtml),
	(".html", convertFile convertHtml),
	(".docx", convertFile convertHtml),
	(".docm", convertFile convertHtml),
	(".xlsx", convertFile convertHtml),
	(".xlsm", convertFile convertHtml),
	(".gz", unpack "gzip" ["-d"]),
	(".bz2", unpack "bunzip2" []),
	(".tar", unpack "tar" ["-xf"]),
	(".zip", unpack "unzip" [])]
