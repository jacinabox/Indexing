{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Graphics.Win32
import System.Win32.DLL
import Data.IORef
import Data.Bits
import Data.Int
import Data.List
import Data.Function
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad
import System.Exit
import Unsafe.Coerce
import System.Environment
import System.Directory
import System.FilePath
import Indexing

foreign import stdcall "windows.h CallWindowProcW" callWindowProc :: FunPtr WindowClosure -> HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT

foreign import stdcall "windows.h GetWindowTextW" c_GetWindowText :: HWND -> LPTSTR -> Int32 -> IO LRESULT

foreign import stdcall "windows.h SetFocus" setFocus :: HWND -> IO HWND

getWindowText wnd = do
	ptr <- mallocForeignPtrBytes 1000
	withForeignPtr ptr $ \p -> do
		c_GetWindowText wnd p 1000
		peekTString p

data Sort = ByPath | ByName | ByExtension

textBoxHeight :: Int32
textBoxHeight = 20

textHeight :: Int32
textHeight = 14

pad :: Int32
pad = 3

gWLP_WNDPROC = -4

subclassProc :: HWND -> (WindowClosure -> WindowClosure) -> IO ()
subclassProc wnd proc = do
	oldProcVar <- newIORef Nothing
	let closure wnd msg wParam lParam = do
		may <- readIORef oldProcVar
		case may of
			Just oldProc -> callWindowProc oldProc wnd msg wParam lParam
			Nothing -> return 0
	closure <- mkWindowClosure (proc closure)
	oldProc <- c_SetWindowLongPtr wnd gWLP_WNDPROC (unsafeCoerce closure)
	writeIORef oldProcVar (Just (unsafeCoerce oldProc))

drawMessage wnd = do
	dc <- getDC (Just wnd)
	font <- getStockFont aNSI_VAR_FONT
	oldFont <- selectFont dc font
	white <- getStockBrush wHITE_BRUSH
	fillRect dc (0, textBoxHeight + 1, 32767, 32767) white
	textOut dc pad (textBoxHeight + pad) "Please wait"
	selectFont dc oldFont
	releaseDC (Just wnd) dc

sortResults sortRef resultsRef = do
	srt <- readIORef sortRef
	(res, nKeywords) <- readIORef resultsRef
	writeIORef resultsRef (sortBy (compare `on` \(s, _) -> case srt of
		ByPath -> s
		ByName -> takeFileName s
		ByExtension -> takeExtension s) res,
		nKeywords)

wndProc :: IORef (Maybe HWND) -> IORef ([(String, [String])], Int32) -> IORef Sort -> IORef Int32 -> HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT
wndProc ref resultsRef sortRef scrollRef wnd msg wParam lParam
	| msg == wM_USER	= do
		if wParam == vK_RETURN then -- This is where we actually do the search
				do
				drawMessage wnd
				Just txt <- readIORef ref
				s <- getWindowText txt
				let keywords = filter ((>2) . length) (parseKeywords s)
				if null keywords then
						writeIORef resultsRef ([], 0)
					else do
						res <- lookKeywords keywords False
						writeIORef resultsRef (map (\(nm, text) -> (nm, contexts keywords text False)) res, fromIntegral $ length keywords)
				sortResults sortRef resultsRef
				writeIORef scrollRef 0
			else if wParam == vK_UP then
				modifyIORef' scrollRef (\x -> 0 `max` (x - 100))
			else do
				(res, nKeywords) <- readIORef resultsRef
				modifyIORef' scrollRef (\x -> (((nKeywords+1)*textHeight+3*pad)*fromIntegral (length res)) `min` (x + 100))
		invalidateRect (Just wnd) Nothing True
		return 0
	| msg == wM_SIZE	= do
		may <- readIORef ref
		case may of
			Just txt -> do
				(_, _, x, y) <- getClientRect wnd
				moveWindow txt 0 0 (fromIntegral x) (fromIntegral textBoxHeight) True
			Nothing -> return ()
		return 0
	| msg == wM_PAINT	= allocaPAINTSTRUCT $ \ps -> do
		dc <- beginPaint wnd ps
		(results, nKeywords) <- readIORef resultsRef
		scroll <- readIORef scrollRef
		white <- getStockBrush wHITE_BRUSH
		font <- getStockFont aNSI_VAR_FONT
		oldFont <- selectFont dc font

		pen <- createPen pS_SOLID 0 (rgb 128 128 128)
		yRef <- newIORef (textBoxHeight - scroll)
		mapM_
			(\(result, contexts) -> do
				y <- readIORef yRef
				let newY = y+textHeight*(nKeywords+1)+3*pad

				fillRect dc (0, y + 1, 32767, newY) white

				textOut dc pad (y + pad) result
				
				modifyIORef' yRef (+(textHeight+2*pad))
				mapM_ (\s -> do
					y <- readIORef yRef
					textOut dc pad y s
					writeIORef yRef (y + textHeight)) contexts

				oldpen <- selectPen dc pen
				moveToEx dc 0 newY
				lineTo dc 32767 newY
				selectPen dc oldpen

				writeIORef yRef newY)
			results
		deletePen pen

		y <- readIORef yRef
		fillRect dc (0, y + 1, 32767, 32767) white

		pen <- createPen pS_SOLID 0 (rgb 0 128 255)
		oldpen <- selectPen dc pen
		moveToEx dc 0 textBoxHeight
		lineTo dc 32767 textBoxHeight
		selectPen dc oldpen
		deletePen pen

		selectFont dc oldFont
		endPaint wnd ps
		return 0
	| msg == wM_CLOSE	= exitSuccess
	| otherwise		= defWindowProc (Just wnd) msg wParam lParam

dLGC_HASSETSEL :: Int32
dLGC_HASSETSEL = 8

dLGC_UNDEFPUSHBUTTON :: Int32
dLGC_UNDEFPUSHBUTTON = 32

dLGC_WANTARROWS :: Int32
dLGC_WANTARROWS = 1

dLGC_WANTCHARS :: Int32
dLGC_WANTCHARS = 128

txtProc parent proc wnd msg wParam lParam
	| msg == wM_KEYUP && (wParam == vK_RETURN || wParam == vK_UP || wParam == vK_DOWN)
		= sendMessage parent wM_USER wParam 0
	| otherwise
		= proc wnd msg wParam lParam

winMain = do
	inst <- getModuleHandle Nothing
	cursor <- loadCursor Nothing iDC_ARROW
	let cls = (0, inst, Nothing, Just cursor, Nothing, Nothing, mkClassName "class")
	registerClass cls
	ref <- newIORef Nothing
	resultsRef <- newIORef ([], 0)
	sortRef <- newIORef ByPath
	scrollRef <- newIORef 0
	wnd <- createWindow (mkClassName "class") "Desktop Search" (wS_VISIBLE .|. wS_OVERLAPPEDWINDOW .|. wS_CLIPCHILDREN) Nothing Nothing Nothing Nothing Nothing Nothing inst (wndProc ref resultsRef sortRef scrollRef)
	txt <- createWindow (mkClassName "Edit") "" (wS_VISIBLE .|. wS_CHILDWINDOW) (Just 0) (Just 0) (Just 100) (Just 10) (Just wnd) Nothing inst (defWindowProc . Just)
	writeIORef ref (Just txt)
	subclassProc txt (txtProc wnd)
	setFocus txt
	sendMessage wnd wM_SIZE 0 0
	font <- getStockFont aNSI_VAR_FONT
	sendMessage txt wM_SETFONT (unsafeCoerce font) 1

	allocaMessage $ \msg ->
		let loop = do
			getMessage msg Nothing
			translateMessage msg
			dispatchMessage msg
			loop in
		loop

main = do
	args <- getArgs
	let keywords = filter ((>2) . length) args
	if not (null args) && head args == "-i" then
		if length args == 1 then
			fullIndex
		else do
			dir <- canonicalizePath (args !! 1)
			indexDirectory dir dir
	else if null keywords then
		winMain
	else do
		let caseSensitive = "-c" `elem` args
		results <- lookKeywords keywords caseSensitive
		mapM_ (\(nm, text) -> do
				putStrLn ""
				putStrLn ("  " ++ nm)
				mapM_ putStrLn (contexts keywords text caseSensitive))
			results
		when (null results) (putStrLn "Index: no results found")

