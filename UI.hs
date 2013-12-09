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
import Control.Monad
import System.Exit
import Unsafe.Coerce
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import Control.Exception
import System.IO
import Foreign.ForeignPtr
import Indexing
import Unpacks
import Split
import Subclass

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

data Sort = ByPath | ByName | ByType

textBoxHeight :: (Integral i) => i
textBoxHeight = 21

textHeight :: (Integral i) => i
textHeight = 14

pad :: Int32
pad = 3

btnWidth :: Int32
btnWidth = 175

gWLP_ID = -12

wM_MOUSEWHEEL = 522

dLGC_WANTARROWS :: Int32
dLGC_WANTARROWS = 1

dLGC_WANTCHARS :: Int32
dLGC_WANTCHARS = 128

cB_INSERTSTRING = 330

cB_ADDSTRING = 323

gW_CHILD = 5

txtId :: Int
txtId = 100

btnId :: Int
btnId = 101

sort1Id :: Int
sort1Id = 102

sort2Id :: Int
sort2Id = 103

sort3Id :: Int
sort3Id = 104

caseId :: Int
caseId = 105

drawMessage wnd = do
	dc <- getDC (Just wnd)
	font <- makeFont fW_NORMAL
	oldFont <- selectFont dc font
	white <- getStockBrush wHITE_BRUSH
	fillRect dc (0, 2 * textBoxHeight, 32767, 32767) white
	textOut dc pad (2 * textBoxHeight + pad) "Please wait"
	selectFont dc oldFont
	deleteFont font
	releaseDC (Just wnd) dc

sortResults settingsRef resultsRef = do
	srt <- readIORef settingsRef
	(res, nKeywords) <- readIORef resultsRef
	writeIORef resultsRef (sortBy (compare `on` \(s, _) -> if srt == sort1Id then
			s
		else if srt == sort2Id then
			takeFileName s
		else
			takeExtension s) res,
		nKeywords)

loWord :: (Integral i, Bits i) => i -> i
loWord n = n .&. 32767

hiWord :: (Bits b) => b -> b
hiWord n = shiftR n 16

logicalCoord i nKeywords = ((nKeywords+1)*textHeight+3*pad+1)*i

screenCoord i nKeywords scroll = logicalCoord i nKeywords + 2 * textBoxHeight - scroll

hitTest :: LPARAM -> IORef (t, Int32) -> IORef Int32 -> IO Int32
hitTest y resultsRef scrollRef = do
	(_, nKeywords) <- readIORef resultsRef
	scroll <- readIORef scrollRef
	return $ (y - 2 * textBoxHeight + scroll) `div` ((nKeywords+1)*textHeight+3*pad+1)

insertString txt s = withTString s $ \p -> sendMessage txt cB_INSERTSTRING 0 (unsafeCoerce p)

openHistory mode = do
	dir <- getAppUserDataDirectory "Index"	
	fl <- openFile (dir ++ pathSeparator : "History.txt") mode
	hSetEncoding fl utf8
	return fl

readHistory txt historyRef = do
	fl <- openHistory ReadMode
	text <- hGetContents fl
	mapM_ (insertString txt) (lines text)
	hClose fl

writeHistory historyRef = do
	fl <- openHistory AppendMode
	history <- readIORef historyRef
	mapM_ (hPutStrLn fl) (reverse history)
	hClose fl

txtProc parent proc wnd msg wParam lParam
	| msg == wM_GETDLGCODE
		= return $ dLGC_WANTCHARS .|. dLGC_WANTARROWS
	| msg == wM_KEYDOWN && wParam `elem` [vK_UP, vK_DOWN, vK_PRIOR, vK_NEXT]
		= sendMessage parent (wM_USER + 1) wParam 0
	| otherwise		= proc wnd msg wParam lParam

doText dc x y s = do
	textOut dc x y s
	(ext, _) <- getTextExtentPoint32 dc s
	white <- getStockBrush wHITE_BRUSH
	fillRect dc (x + ext, y, 32767, y + textHeight) white

makeFont weight = catch
	(createFont 13 0 0 0 weight False False False dEFAULT_CHARSET oUT_DEFAULT_PRECIS cLIP_DEFAULT_PRECIS dEFAULT_QUALITY fF_DONTCARE "Tahoma")
	(\(_ :: SomeException) -> getStockFont aNSI_VAR_FONT)

quote s = "\"" ++ s ++ "\""

wndProc :: IORef ([(String, [String])], Int32) -> IORef Int -> IORef Int32 -> IORef [String] -> HWND -> UINT -> WPARAM -> LPARAM -> IO Int
wndProc resultsRef settingsRef scrollRef historyRef wnd msg wParam lParam
	-- Handlers for commands involving individual results
	| msg == wM_COMMAND && loWord wParam == fromIntegral btnId	= do
		-- Do a hit test on the control's position to determine which folder to open.
		btn <- getDlgItem wnd btnId
		(_, _, _, y) <- getWindowRect btn
		(_, y) <- screenToClient wnd (0, y)
		i <- hitTest y resultsRef scrollRef
		(res, _) <- readIORef resultsRef
		when (i < fromIntegral (length res)) $ do
			createProcess (proc "explorer" [takeDirectory $ head $ split '@' $ fst $ res !! fromIntegral i])
			return ()
		return 0
	| msg == wM_COMMAND && loWord wParam `elem` map fromIntegral [sort1Id, sort2Id, sort3Id]	= do
		writeIORef settingsRef (fromIntegral (loWord wParam))
		sortResults settingsRef resultsRef
		invalidateRect (Just wnd) Nothing True
		return 0
	| msg == wM_LBUTTONDBLCLK	= do
		(res, _) <- readIORef resultsRef
		i <- hitTest (hiWord lParam) resultsRef scrollRef
		when (i < fromIntegral (length res)) $ do
			createProcess (shell $ quote $ head $ split '@' $ fst $ res !! fromIntegral i)
			return ()
		return 0
	| msg == wM_LBUTTONUP || msg == wM_RBUTTONUP	= do
		txt <- getDlgItem wnd txtId
		setFocus txt
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
	| msg == wM_COMMAND && wParam == iDOK	= do
		txt <- getDlgItem wnd txtId
		btn <- getDlgItem wnd btnId
		res <- c_IsDlgButtonChecked wnd caseId
		showWindow btn sW_HIDE
		drawMessage wnd
		s <- getWindowText txt
		let keywords = filter ((>2) . length) (parseKeywords s)
		setWindowText wnd (s ++ " - Desktop Search")
		if null keywords then
				writeIORef resultsRef ([], 0)
			else do
				insertString txt s
				modifyIORef' historyRef (s:)
				res <- lookKeywords keywords (res /= 0)
				writeIORef resultsRef (res, fromIntegral $ length keywords)
		sortResults settingsRef resultsRef
		writeIORef scrollRef 0
		invalidateRect (Just wnd) Nothing True
		return 1

	-- Handler relating to scrolling
	| msg == wM_USER + 1	= do
		txt <- getDlgItem wnd txtId
		btn <- getDlgItem wnd btnId
		showWindow btn sW_HIDE
		(_, _, _, y) <- getClientRect wnd
		let offset = fromJust $ lookup wParam [(vK_UP, -100), (vK_DOWN, 100), (vK_PRIOR, -y), (vK_NEXT, y)]
		(res, nKeywords) <- readIORef resultsRef
		scroll <- readIORef scrollRef
		let newScroll = logicalCoord (fromIntegral (length res)-1) nKeywords `min` (0 `max` (scroll + offset))
		writeIORef scrollRef newScroll
		when (scroll /= newScroll) (invalidateRect (Just wnd) Nothing True)
		return 1

	-- Miscellaneous handlers
	| msg == wM_INITDIALOG	= do
		inst <- getModuleHandle Nothing
		txt <- createWindow (mkClassName "ComboBox") "" (wS_VISIBLE .|. wS_CHILDWINDOW .|. cBS_HASSTRINGS .|. cBS_DROPDOWN) (Just 0) (Just 0) (Just 100) (Just 10) (Just wnd) Nothing inst (defWindowProc . Just)
		btn <- createWindow (mkClassName "Button") "Open containing folder" wS_CHILDWINDOW (Just 0) (Just 0) (Just 100) (Just 10) (Just wnd) Nothing inst (defWindowProc . Just)
		sort1 <- createWindow (mkClassName "Button") "Sort by path" (wS_VISIBLE .|. wS_CHILDWINDOW .|. bS_AUTORADIOBUTTON) (Just 0) (Just textBoxHeight) (Just 100) (Just textBoxHeight) (Just wnd) Nothing inst (defWindowProc . Just)
		sort2 <- createWindow (mkClassName "Button") "Sort by name" (wS_VISIBLE .|. wS_CHILDWINDOW .|. bS_AUTORADIOBUTTON) (Just 100) (Just textBoxHeight) (Just 100) (Just textBoxHeight) (Just wnd) Nothing inst (defWindowProc . Just)
		sort3 <- createWindow (mkClassName "Button") "Sort by type" (wS_VISIBLE .|. wS_CHILDWINDOW .|. bS_AUTORADIOBUTTON) (Just 200) (Just textBoxHeight) (Just 100) (Just textBoxHeight) (Just wnd) Nothing inst (defWindowProc . Just)
		caseBtn <- createWindow (mkClassName "Button") "Case sensitive" (wS_VISIBLE .|. wS_CHILDWINDOW .|. bS_AUTOCHECKBOX) (Just 300) (Just textBoxHeight) (Just 32767) (Just textBoxHeight) (Just wnd) Nothing inst (defWindowProc . Just)
		c_SetWindowLongPtr txt gWLP_ID (unsafeCoerce txtId)
		c_SetWindowLongPtr btn gWLP_ID (unsafeCoerce btnId)
		c_SetWindowLongPtr sort1 gWLP_ID (unsafeCoerce sort1Id)
		c_SetWindowLongPtr sort2 gWLP_ID (unsafeCoerce sort2Id)
		c_SetWindowLongPtr sort3 gWLP_ID (unsafeCoerce sort3Id)
		c_SetWindowLongPtr caseBtn gWLP_ID (unsafeCoerce caseId)
		readHistory txt historyRef
		font <- makeFont fW_NORMAL
		edit <- getWindow txt gW_CHILD
		subclassProc edit (txtProc wnd)
		mapM_
			(\wnd -> sendMessage wnd wM_SETFONT (unsafeCoerce font) 1)
			[txt, btn, sort1, sort2, sort3, caseBtn]
		checkRadioButton wnd sort1Id sort3Id sort1Id
		setFocus txt
		return 0
	| msg == wM_SIZE	= catch
		(do
		txt <- getDlgItem wnd txtId
		btn <- getDlgItem wnd btnId
		(_, _, x, y) <- getClientRect wnd
		moveWindow txt 0 0 (fromIntegral x) textBoxHeight True
		showWindow btn sW_HIDE
		return 0)
		(\(_ :: SomeException) -> return 0)
	| msg == wM_CTLCOLORDLG	= liftM unsafeCoerce (getStockBrush nULL_BRUSH)
	| msg == wM_PAINT	= allocaPAINTSTRUCT $ \ps -> do
		dc <- beginPaint wnd ps
		(results, nKeywords) <- readIORef resultsRef
		scroll <- readIORef scrollRef
		white <- getStockBrush wHITE_BRUSH
		font <- makeFont fW_NORMAL
		fontBold <- makeFont fW_BOLD
		oldFont <- selectFont dc fontBold

		pen <- createPen pS_SOLID 0 (rgb 192 192 192)
		yRef <- newIORef (2 * textBoxHeight - scroll)
		mapM_
			(\(result, contexts) -> do
				y <- readIORef yRef
				let newY = y+textHeight*(nKeywords+1)+3*pad+1

				oldpen <- selectPen dc pen
				moveToEx dc 0 y
				lineTo dc 32767 y
				selectPen dc oldpen

				fillRect dc (0, y + 1, 32767, newY) white

				textOut dc pad (y + pad + 1) result
				
				selectFont dc font
				modifyIORef' yRef (+(textHeight+2*pad+1))
				mapM_ (\s -> do
					y <- readIORef yRef
					textOut dc pad y s
					writeIORef yRef (y + textHeight)) contexts
				selectFont dc fontBold

				writeIORef yRef newY)
			results
		deletePen pen

		y <- readIORef yRef
		oldpen <- selectPen dc pen
		moveToEx dc 0 y
		lineTo dc 32767 y
		selectPen dc oldpen
		fillRect dc (0, y + 1, 32767, 32767) white

		selectFont dc oldFont
		deleteFont font
		deleteFont fontBold
		endPaint wnd ps
		return 0
	| msg == wM_CLOSE || msg == wM_ENDSESSION	= do
		writeHistory historyRef
		exitSuccess
	| otherwise		= return 0

winMain = do
	resultsRef <- newIORef ([], 0)
	settingsRef <- newIORef sort1Id
	scrollRef <- newIORef 0
	historyRef <- newIORef []
	inst <- getModuleHandle Nothing
	let tmp = DialogTemplate 0 0 640 480 (wS_VISIBLE .|. wS_OVERLAPPEDWINDOW .|. wS_CLIPCHILDREN) 0 (Left 0) (Left 0) (Right "Desktop Search") (Left 0) 14
		[]
	tmp2 <- mkDialogFromTemplate tmp
	dialogBoxIndirect inst tmp2 Nothing (wndProc resultsRef settingsRef scrollRef historyRef)

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
			indexWrapper (appendDelimiter dir)
	else if null keywords then
		winMain
	else do
		let caseSensitive = "-c" `elem` args
		results <- lookKeywords keywords caseSensitive
		mapM_ (\(nm, contexts) -> do
				putStrLn ""
				putStrLn ("  " ++ nm)
				mapM_ putStrLn contexts)
			results
		when (null results) (putStrLn "Index: no results found")
