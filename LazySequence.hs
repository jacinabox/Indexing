{-# LANGUAGE TupleSections, ExistentialQuantification #-}

module LazySequence (lazyUnfoldrM, lazySequence, lazyMapM, lazyGetContents) where

import Data.IORef
import System.IO
import System.IO.Unsafe
import Control.Monad
import Control.Monad.Writer
import Control.Exception
import Control.Parallel.Strategies

unsafeUnfoldr f g x = do
	ref <- newIORef []
	let unfold y = g y >>= maybe
		(return [])
		(\(z, a) -> do
			pos <- liftM (z:) $ unsafeInterleaveIO (unfold a)
			writeIORef ref pos
			return pos)
	x <- unfold x
	y <- liftM (`using` evalList rseq) (f x)
	evaluate y
	pos <- readIORef ref
	evaluate (pos `using` evalList rseq)
	return y

-- | Equal to 'liftM f (unfoldrM g x)' but constructs the intermediate result lazily.
lazyUnfoldrM f = unsafeUnfoldr (return . f)

-- | Equal to 'liftM f . sequence'
lazySequence f = lazyUnfoldrM f (\ls -> case ls of
	x:xs -> liftM (Just . (, xs)) x
	[] -> return Nothing)

lazyMapM f g = lazySequence f . map g

-- | Gets the contents of a file, but is semantically sound.
lazyGetContents hdl f = do
	contents <- hGetContents hdl
	finally
		(lazySequence f (map (return $!) contents))
		(hClose hdl)
