module Replace where

import Data.List
import Control.Monad

replace assocs (c:cs) =
	let stripped = map (\(vr, with) -> fmap ((,) with) (stripPrefix vr (c:cs))) assocs in
	case foldl mplus mzero stripped of
		Just (with, s) -> with ++ replace assocs s
		Nothing -> c : replace assocs cs
replace _ [] = []
