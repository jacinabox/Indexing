{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses #-}

module Unpacks (nullDevice, getFile, unpacks, pathDelimiter, appendDelimiter, customQuery,{- Rec(Rec),-} database, getTable, record) where

import Data.List
import Data.Char
import Data.Maybe
import System.Directory
import System.Process
import System.FilePath
import Control.Exception
import Control.Monad
import Control.Monad.Maybe
-- import Database.HaskellDB
-- import Database.HaskellDB.Database
-- import Database.HaskellDB.PrimQuery
--import Database.HaskellDB.HSQL.MySQL
import Prelude hiding (catch)

import Replace
import Split

#ifdef WIN32
pathDelimiter = '\\'

nullDevice = "NUL"
#else
pathDelimiter = '/'

nullDevice = "/dev/null"
#endif

appendDelimiter s
	| last s `elem` "\\/"	= s
	| otherwise		= s ++ [pathDelimiter]

convertHtml0 prev ('<':xs) = maybe "" reverse prev ++ convertHtml0 Nothing xs
convertHtml0 _ ('>':xs) = convertHtml0 (Just "") xs
convertHtml0 prev ('&':'#':xs) = convertHtml0 (fmap (chr (read num):) prev) (tail rest)
	where (num, rest) = span isDigit xs
convertHtml0 prev (x:xs) = convertHtml0 (fmap (x:) prev) xs
convertHtml0 prev [] = maybe "" reverse prev

conversionTable = [("&nbsp;", ' '),
	("&lt;", '<'),
	("&gt;", '>'),
	("&amp;", '&'),
	("&quot;", '"'),
	("&Agrave;", chr 192),
	("&Aacute;", chr 193),
	("&Acirc;", chr 194),
	("&Atilde;", chr 195),
	("&Auml;", chr 196),
	("&Aring;", chr 197),
	("&AElig;", chr 198),
	("&Ccedil;", chr 199),
	("&Egrave;", chr 200),
	("&Eacute;", chr 201),
	("&Ecirc;", chr 202),
	("&Euml;", chr 203),
	("&lgrave;", chr 204),
	("&lacute;", chr 205),
	("&lcirc;", chr 206),
	("&luml;", chr 207),
	("&ETH;", chr 208),
	("&Ntilde;", chr 209),
	("&Ograve;", chr 210),
	("&Oacute;", chr 211),
	("&Ocirc;", chr 212),
	("&Otilde;", chr 213),
	("&Ouml;", chr 214),
	("&Oslash;", chr 216),
	("&Ugrave;", chr 217),
	("&Uacute;", chr 218),
	("&Ucirc;", chr 219),
	("&Uuml;", chr 220),
	("&Yacute;", chr 221),
	("&agrave;", chr 224),
	("&aacute;", chr 225),
	("&acirc;", chr 226),
	("&atilde;", chr 227),
	("&auml;", chr 228),
	("&aring;", chr 229),
	("&aelig;", chr 230),
	("&ccedil;", chr 231),
	("&egrave;", chr 232),
	("&eacute;", chr 233),
	("&ecirc;", chr 234),
	("&euml;", chr 235),
	("&igrave;", chr 236),
	("&iacute;", chr 237),
	("&icirc;", chr 238),
	("&iuml;", chr 239),
	("&eth;", chr 240),
	("&ntilde;", chr 241),
	("&ograve;", chr 242),
	("&oacute;", chr 243),
	("&ocirc;", chr 244),	
	("&otilde;", chr 245),
	("&ouml;", chr 246),
	("&ugrave;", chr 249),
	("&uacute;", chr 250),
	("&ucirc;", chr 251),
	("&uuml;", chr 252),
	("&yacute;", chr 253)]

convertHtml = replace (map (\(repl, with) -> (repl, [with])) conversionTable) . convertHtml0 (Just "")

convertRtf0 count ('\\':'{':xs) = '{' : convertRtf0 count xs
convertRtf0 count ('\\':'}':xs) = '}' : convertRtf0 count xs
convertRtf0 count ('{':xs) = convertRtf0 (count + 1) xs
convertRtf0 count ('}':xs) = convertRtf0 (count - 1) xs
convertRtf0 count (x:xs) = if count > 0 then convertRtf0 count xs else x : convertRtf0 count xs
convertRtf0 _ [] = []

eraseTags ('\\':'\\':xs) = '\\' : eraseTags xs
eraseTags ('\\':xs) = case snd $ break (`elem` "\\\n ") xs of
	' ' : ys -> eraseTags ys
	ys -> eraseTags ys
eraseTags (x:xs) = x : eraseTags xs
eraseTags [] = []

convertRtf = eraseTags . convertRtf0 0 . tail . init

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
	curdir <- getCurrentDirectory
	setCurrentDirectory path
	readProcess cmd (switches ++ [takeFileName name]) ""
	setCurrentDirectory curdir
	catch (removeFile path2) (\(_ :: IOError) -> return ())
	return path

getFile logicalPath = foldM (\physical next -> maybe (return physical) (liftM (++ next) . ($ physical)) (lookup (takeExtension physical) unpacks)) x xs where
	x:xs = split '@' logicalPath

type Identifier = String

-- A table of conversion functions, that take their filenames
-- as strings and unpack them into temporary directories.
unpacks = [(".htm", convertFile convertHtml),
	(".html", convertFile convertHtml),
	(".docx", convertFile convertHtml),
	(".docm", convertFile convertHtml),
	(".xlsx", convertFile convertHtml),
	(".xlsm", convertFile convertHtml),
	(".rtf", convertFile convertRtf),
	(".gz", unpack "gzip" ["-d"]),
	(".bz2", unpack "bunzip2" []),
	(".tar", unpack "tar" ["-xf"]),
	(".zip", unpack "unzip" [])]

-- A file interface for SQL databases, answering to the phony protocol sql://server/user@password/db/table/key.
connect ident f = undefined{-mysqlConnect (MySQLOptions svr db usr pwd) f where
	(svr, rest) = break (==pathDelimiter) (drop 6 ident)
	(usr, rest1) = break (=='@') (drop 1 rest)
	(pwd, rest2) = break (==pathDelimiter) (drop 1 rest1)
	db = takeWhile (/=pathDelimiter) (drop 1 rest2)-}

customQuery db tab sql = undefined{-do
	(col, _):_ <- describe tab
	query db $ restrict (col .==. literal ("''; " ++ sql)) (table tab)-}

{-primaryKey db tab = customQuery db tab $ "SHOW KEYS FROM `" ++ tab ++ "` WHERE Key_name = 'PRIMARY'"

-- | A typeless record type, for use when the record type is not known in advance.
newtype Rec t = Rec [(Attribute, Maybe String)] deriving (Read, Show, Eq, Ord)

liftMay m = m >>= maybe mzero return

instance GetRec (Rec t) (Rec t) where
	getRec insts _ scheme x _ = liftM Rec $ mapM
		(\col -> runMaybeT $ liftM show (liftMay $ getString x col)
			`mplus` liftM show (liftMay $ getInt x col)
			`mplus` liftM show (liftMay $ getInteger x col)
			`mplus` liftM show (liftMay $ getDouble x col)
			`mplus` liftM show (liftMay $ getBool x col)
			`mplus` liftM show (liftMay $ getCalendarTime x col)
			`mplus` liftM show (liftMay $ getLocalTime x col))
		scheme-}

database ident = undefined -- connect ident tables

getTable ident tab = undefined{-connect ident (\db -> do
	primary:_ <- primaryKey db tab
	query db (table tab))-}

record ident tab key = undefined{-connect ident (\db -> do
	primary:_ <- primaryKey tab
	query db (restrict (table tab) (primary .==. key)))-}

