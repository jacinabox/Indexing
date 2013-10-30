{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Main (main) where

import Graphics.Win32
import System.Win32.DLL
import Data.IORef
import Data.Bits
import Data.Int
import Data.List
import Data.Maybe
import Data.Function
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad
import System.Exit
import Unsafe.Coerce
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import Control.Exception
import System.IO
import Indexing
import Split

foreign import stdcall "windows.h CallWindowProcW" callWindowProc :: FunPtr WindowClosure -> HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT

foreign import stdcall "windows.h GetWindowTextW" c_GetWindowText :: HWND -> LPTSTR -> Int32 -> IO LRESULT

foreign import stdcall "windows.h SetFocus" setFocus :: HWND -> IO HWND

foreign import stdcall "windows.h GetWindow" getWindow :: HWND -> UINT -> IO HWND

foreign import stdcall "windows.h DrawTextW" c_DrawText :: HDC -> LPTSTR -> Int32 -> LPRECT -> UINT -> IO Int32

drawText dc s rt format = withTString s $ \ps ->
	withRECT rt $ \pr ->
		c_DrawText dc ps (-1) pr format

getWindowText wnd = do
	ptr <- mallocForeignPtrBytes 1000
	withForeignPtr ptr $ \p -> do
		c_GetWindowText wnd p 1000
		peekTString p

data Sort = ByPath | ByName | ByExtension

textBoxHeight :: Int32
textBoxHeight = 19

textHeight :: Int32
textHeight = 13

pad :: Int32
pad = 3

btnWidth :: Int32
btnWidth = 175

gWLP_ID = -12

gWLP_WNDPROC = -4

wM_MOUSEWHEEL = 522

dLGC_WANTARROWS = 1

cB_INSERTSTRING = 330

cB_ADDSTRING = 323

gW_CHILD = 5

dLGC_WANTALLKEYS = 4

txtId :: Int
txtId = 100

btnId :: Int
btnId = 101

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

loWord :: LPARAM -> LPARAM
loWord n = n .&. 32767

hiWord :: LPARAM -> LPARAM
hiWord n = shiftR n 16

screenCoord i nKeywords scroll = ((nKeywords+1)*textHeight+3*pad)*i+textBoxHeight-scroll

hitTest :: LPARAM -> IORef (t, Int32) -> IORef Int32 -> IO Int32
hitTest y resultsRef scrollRef = do
	(_, nKeywords) <- readIORef resultsRef
	scroll <- readIORef scrollRef
	return $ (y - textBoxHeight + scroll) `div` ((nKeywords+1)*textHeight+3*pad)

addString txt s = withTString s $ \p -> sendMessage txt cB_ADDSTRING 0 (unsafeCoerce p)

insertString txt s = withTString s $ \p -> sendMessage txt cB_INSERTSTRING 0 (unsafeCoerce p)

openHistory mode = do
	dir <- getAppUserDataDirectory "Index"	
	openFile (dir ++ pathSeparator : "History.txt") mode

readHistory txt historyRef = do
	fl <- openHistory ReadMode
	text <- hGetContents fl
	mapM_ (addString txt) (lines text)
	hClose fl
	writeIORef historyRef (lines text)

writeHistory historyRef = do
	fl <- openHistory WriteMode
	history <- readIORef historyRef
	mapM_ (hPutStrLn fl) (take 20 history)
	hClose fl

txtProc parent proc wnd msg wParam lParam
	| msg == wM_GETDLGCODE	= return dLGC_WANTALLKEYS
	| msg == wM_KEYDOWN && wParam == vK_RETURN
		= sendMessage parent (wM_USER + 1) 0 0
	| msg == wM_KEYDOWN && wParam `elem` [vK_UP, vK_DOWN, vK_PRIOR, vK_NEXT]
		= sendMessage parent (wM_USER + 2) wParam 0
	| otherwise		= proc wnd msg wParam lParam

doText dc x y s = do
	textOut dc x y s
	(ext, _) <- getTextExtentPoint32 dc s
	white <- getStockBrush wHITE_BRUSH
	fillRect dc (x + ext, y, 32767, y + textHeight) white

quote s = "\"" ++ s ++ "\""

wndProc :: IORef ([(String, [String])], Int32) -> IORef Sort -> IORef Int32 -> IORef [String] -> HWND -> UINT -> WPARAM -> LPARAM -> IO Int
wndProc resultsRef sortRef scrollRef historyRef wnd msg wParam lParam
	-- Handlers for commands involving individual results
	| msg == wM_COMMAND && loWord (fromIntegral wParam) == fromIntegral btnId	= do
		-- Do a hit test on the control's position to determine which folder to open.
		do
		btn <- getDlgItem wnd btnId
		(_, _, _, y) <- getWindowRect btn
		(_, y) <- screenToClient wnd (0, y)
		i <- hitTest y resultsRef scrollRef
		(res, _) <- readIORef resultsRef
		when (i < fromIntegral (length res)) $ do
			createProcess (proc "explorer" [takeDirectory $ head $ split '@' $ fst $ res !! fromIntegral i])
			return ()
		return 0
	| msg == wM_LBUTTONUP	= do
		(res, _) <- readIORef resultsRef
		i <- hitTest (hiWord lParam) resultsRef scrollRef
		when (i < fromIntegral (length res)) $ do
			createProcess (shell $ quote $ head $ split '@' $ fst $ res !! fromIntegral i)
			return ()
		return 0
	| msg == wM_MOUSEMOVE	= do
		(res, nKeywords) <- readIORef resultsRef
		scroll <- readIORef scrollRef
		i <- hitTest (hiWord lParam) resultsRef scrollRef
		when (i < fromIntegral (length res)) $ do
			(_, _, x, y) <- getClientRect wnd
			btn <- getDlgItem wnd btnId
			moveWindow btn (fromIntegral (x - btnWidth - pad)) (fromIntegral $ screenCoord i nKeywords scroll + pad + 1) (fromIntegral btnWidth) 20 True
			showWindow btn sW_SHOW
			return ()
		return 0

	-- This is where we actually do the search
	| msg == wM_USER + 1	= do
		drawMessage wnd
		txt <- getDlgItem wnd txtId
		s <- getWindowText txt
		setWindowText wnd (s ++ " - Desktop Search")
		insertString txt s
		modifyIORef' historyRef (s:)
		let keywords = filter ((>2) . length) (parseKeywords s)
		if null keywords then
				writeIORef resultsRef ([], 0)
			else do
				res <- lookKeywords keywords False
				writeIORef resultsRef (map (\(nm, text) -> (nm, contexts keywords text False)) res, fromIntegral $ length keywords)
		sortResults sortRef resultsRef
		writeIORef scrollRef 0
		invalidateRect (Just wnd) Nothing True
		return 0

	-- Handler relating to scrolling
	| msg == wM_USER + 2	= do
		txt <- getDlgItem wnd txtId
		btn <- getDlgItem wnd btnId
		showWindow btn sW_HIDE
		(_, _, _, y) <- getClientRect wnd
		let offset = fromJust $ lookup wParam [(vK_UP, -100), (vK_DOWN, 100), (vK_PRIOR, -y), (vK_NEXT, y)]
		(res, nKeywords) <- readIORef resultsRef
		modifyIORef' scrollRef (\x -> (((nKeywords+1)*textHeight+3*pad)*(fromIntegral (length res)-1)) `min` (0 `max` (x + offset)))
		invalidateRect (Just wnd) Nothing True
		return 0

	-- Miscellaneous handlers
 	| msg == wM_SETFOCUS	= do
		txt <- getDlgItem wnd txtId
		setFocus txt
		return 0
	| msg == wM_INITDIALOG	= do
		inst <- getModuleHandle Nothing
		txt <- createWindow (mkClassName "ComboBox") "" (wS_VISIBLE .|. wS_CHILDWINDOW .|. cBS_HASSTRINGS .|. cBS_DROPDOWN) (Just 0) (Just 0) (Just 100) (Just 10) (Just wnd) Nothing inst (defWindowProc . Just)
		btn <- createWindow (mkClassName "Button") "Open containing folder" wS_CHILDWINDOW (Just 0) (Just 0) (Just 100) (Just 10) (Just wnd) Nothing inst (defWindowProc . Just)
		c_SetWindowLongPtr txt gWLP_ID (unsafeCoerce txtId)
		c_SetWindowLongPtr btn gWLP_ID (unsafeCoerce btnId)
		readHistory txt historyRef
		font <- getStockFont aNSI_VAR_FONT
		edit <- getWindow txt gW_CHILD
		subclassProc edit (txtProc wnd)
		sendMessage txt wM_SETFONT (unsafeCoerce font) 1
	        sendMessage btn wM_SETFONT (unsafeCoerce font) 1
		setFocus txt
		return 0
	| msg == wM_SIZE	= catch
		(do
		txt <- getDlgItem wnd txtId
		btn <- getDlgItem wnd btnId
		(_, _, x, y) <- getClientRect wnd
		moveWindow txt 0 0 (fromIntegral x) 300 True
		showWindow btn sW_HIDE
		return 0)
		(\(_ :: SomeException) -> return 0)
	| msg == wM_CTLCOLORDLG	= liftM unsafeCoerce (getStockBrush nULL_BRUSH)
	| msg == wM_PAINT	= allocaPAINTSTRUCT $ \ps -> do
		dc <- beginPaint wnd ps
		(results, nKeywords) <- readIORef resultsRef
		scroll <- readIORef scrollRef
		white <- getStockBrush wHITE_BRUSH
		font <- getStockFont aNSI_VAR_FONT
		oldFont <- selectFont dc font

		pen <- createPen pS_SOLID 0 (rgb 192 192 192)
		yRef <- newIORef (textBoxHeight - scroll)
		mapM_
			(\(result, contexts) -> do
				y <- readIORef yRef
				let newY = y+textHeight*(nKeywords+1)+3*pad

				fillRect dc (0, y + 1, 32767, y + pad) white
				fillRect dc (0, y + 1, pad, newY - 1) white

				doText dc pad (y + pad) result

				fillRect dc (0, y + pad + textHeight, 32767, y + 2 * pad + textHeight) white
                                
                                modifyIORef' yRef (+(textHeight+2*pad))
                                mapM_ (\s -> do
                                        y <- readIORef yRef
                                        doText dc pad y s
                                        writeIORef yRef (y + textHeight)) contexts

				fillRect dc (0, newY - pad, 32767, newY) white

				oldpen <- selectPen dc pen
				moveToEx dc 0 newY
				lineTo dc 32767 newY
				selectPen dc oldpen

				writeIORef yRef newY)
			results
		deletePen pen

		y <- readIORef yRef
		fillRect dc (0, y + 1, 32767, 32767) white

		selectFont dc oldFont
		endPaint wnd ps
		return 0
	| msg == wM_CLOSE	= do
		writeHistory historyRef
		exitSuccess
	| otherwise		= return 0

winMain = do
	inst <- getModuleHandle Nothing
	resultsRef <- newIORef ([], 0)
	sortRef <- newIORef ByPath
	scrollRef <- newIORef 0
	historyRef <- newIORef []
	let tmp = DialogTemplate 0 0 640 480 (wS_VISIBLE .|. wS_OVERLAPPEDWINDOW .|. wS_CLIPCHILDREN) 0 (Left 0) (Left 0) (Right "Desktop Search") (Left 0) 14
		[]
	tmp2 <- mkDialogFromTemplate tmp
	dialogBoxIndirect inst tmp2 Nothing (wndProc resultsRef sortRef scrollRef historyRef)

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

