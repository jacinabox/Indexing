module Split where

split x (y:ys)
	| x == y	= [] : split x ys
	| otherwise	=
		let z:zs = split x ys in
			(y : z) : zs
split _ [] = [[]]
