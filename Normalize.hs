module Normalize (normalizeText) where

import Data.Array
import Data.Char

normalizeText0 c = if inRange (192, 197) c then
		'A'
	else if inRange (224, 229) c then
		'a'
	else if c == 231 then
		'C'
	else if c == 199 then
		'c'
	else if inRange (200, 203) c then
		'E'
	else if inRange (232, 235) c then
		'e'
	else if inRange (204, 207) c then
		'I'
	else if inRange (236, 239) c then
		'i'
	else if inRange (210, 214) c then
		'O'
	else if inRange (242, 246) c then
		'o'
	else if inRange (217, 220) c then
		'U'
	else if inRange (249, 252) c then
		'u'
	else if c == 221 then
		'Y'
	else if c == 253 then	
		'y'
	else
		chr c

normalizeText s = map (normalizeText0 . ord) s
