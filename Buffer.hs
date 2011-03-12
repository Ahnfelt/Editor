module Buffer where

import Prelude hiding (length, lines)
import qualified Data.Sequence as S
import Data.Foldable (toList)

data Position = Position {
    offset :: Int,
    line :: Int,
    column :: Int
    }

start = Position {
    offset = 0,
    line = 1,
    column = 1
    }

data Buffer = Buffer {
    filename :: Maybe String,
    text :: S.Seq Char,
    scroll :: Position,
    cursor :: Position
    }

empty = Buffer {
    filename = Nothing,
    text = S.empty,
    scroll = start,
    cursor = start
    }

length buffer = S.length (text buffer)

lines buffer = lines' (S.drop (offset $ scroll buffer) $ text buffer) 
    where
        lines' text = 
            let (line, rest) = S.spanl (/= '\n') text in 
            toList (S.drop (column (scroll buffer) - 1) line) : lines' (S.drop 1 rest)

currentLine buffer = drop 1 $ toList $ S.takeWhileR (/= '\n') $ S.take (offset $ cursor buffer) $ text buffer

index i buffer = if i >= 0 && i < S.length (text buffer) then S.index (text buffer) i else ' '

