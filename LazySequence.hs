module LazySequence where

import Data.IORef
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Control.Parallel.Strategies

-- | Equal to 'liftM f (sequence ls)' but constructs the intermediate result lazily.
lazySequence f ls = do
	ref <- newIORef undefined
	x <- foldr (\m m2 -> do
			pos <- liftM2 (:) m (unsafeInterleaveIO m2)
			writeIORef ref pos
			return pos)
		(return [])
		ls
	let y = f x `using` evalList rseq
	evaluate y
	pos <- readIORef ref
	evaluate (pos `using` evalList rseq)
	return y

lazyMapM f g = lazySequence f . fmap g
