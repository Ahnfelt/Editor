module Buffer.Move where

import Buffer

import qualified Data.Sequence as S
import Data.Foldable (toList)

tab buffer = let count = 4 - (column (cursor buffer) - 1) `mod` 4 in
    iterate (insert ' ') buffer !! count

delete buffer = buffer {
    text = left S.>< S.drop 1 right
    }
    where
        (left, right) = S.splitAt (offset $ cursor buffer) (text buffer)

deleteLeft buffer | offset (cursor buffer) > 0 = delete (moveLeft buffer)
deleteLeft buffer = buffer

insert c buffer = moveRight $ buffer {
    text = left S.>< S.singleton c S.>< right
    }
    where
        (left, right) = S.splitAt (offset $ cursor buffer) (text buffer)

inserts "" buffer = buffer
inserts (c:s) buffer = inserts s (insert c buffer)

moveRight buffer = buffer { cursor = (cursor buffer) { 
    offset = offset'',
    column = column' + if line' == 1 then 1 else 0,
    line = line'
    } }
    where
        line' = line (cursor buffer) + if before == '\n' then 1 else 0
        column' = S.length (S.takeWhileR (/= '\n') (S.take offset'' (text buffer)))
        offset'' = min (S.length (text buffer)) (offset' + 1)
        offset' = offset (cursor buffer)
        before = index offset' buffer
        after = index (offset' + 1) buffer

moveLeft buffer = buffer { cursor = (cursor buffer) { 
    offset = offset'',
    column = column' + if line' == 1 then 1 else 0,
    line = line'
    } }
    where
        line' = line (cursor buffer) - if after == '\n' then 1 else 0
        column' = S.length (S.takeWhileR (/= '\n') (S.take offset'' (text buffer)))
        offset'' = max 0 (offset' - 1)
        offset' = offset (cursor buffer)
        before = index offset' buffer
        after = index (offset' - 1) buffer

moveUp buffer = move' buffer False (column $ cursor buffer)
    where
        move' buffer True column' | (column $ cursor buffer) <= column' = buffer
        move' buffer _ _ | (offset $ cursor buffer) == 0 = buffer
        move' buffer line' column' = 
            let buffer' = moveLeft buffer in
            let line'' = line' || S.index (text buffer') (offset $ cursor buffer') == '\n' in
            move' buffer' line'' column'

moveDown buffer = move' buffer False (column $ cursor buffer)
    where
        move' buffer True column' | (column $ cursor buffer) >= column' = buffer
        move' buffer _ _ | (offset $ cursor buffer) == S.length (text buffer) = buffer
        move' buffer line' column' = 
            let buffer' = moveRight buffer in
            let before = index (offset $ cursor buffer) buffer == '\n' in
            let after = index (offset $ cursor buffer') buffer' == '\n' in
            let line'' = line' || before in
            if (line' && after) || (before && after) then buffer' else move' buffer' line'' column'

moveHome _ buffer | offset (cursor buffer) == 0 = buffer
moveHome False buffer | index (offset (cursor buffer) - 1) buffer == '\n' = buffer
moveHome all buffer = moveHome all $ moveLeft buffer

moveEnd _ buffer | offset (cursor buffer) == S.length (text buffer) = buffer
moveEnd False buffer | index (offset (cursor buffer)) buffer == '\n' = buffer
moveEnd all buffer = moveEnd all $ moveRight buffer

