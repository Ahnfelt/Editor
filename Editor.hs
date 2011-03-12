import Graphics.Vty
import Data.Word
import Data.Maybe
import Data.Foldable (toList)
import Data.List
import qualified Data.CircularList as C
import qualified Data.Sequence as S
import qualified Buffer as B
import qualified Buffer.Move as B

data Editor = Editor {
    vt :: Vty,
    width :: Int,
    height :: Int,
    notification :: Maybe (Either (String, String) String),
    buffers :: C.CList B.Buffer,
    wrapping :: Maybe Bool
    }

f `active` editor = case C.focus (buffers editor) of
    Nothing -> editor
    Just buffer -> editor { buffers = C.update (f buffer) (buffers editor) }

main = do
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    loop Editor {
        vt = vt,
        width = fromIntegral w,
        height = fromIntegral h,
        notification = Just (Left (" Press Alt+H for help", " Hax 0.0.1 ")),
        buffers = C.fromList [
            B.empty {B.filename = Just "/home/ahnfelt/DeleteMe.hs", B.text = S.fromList "add :: Int -> Int -> Int\nadd x y = x + y\n\nblah blah\nfoo bar\nbaz\n"}, 
            B.empty {B.filename = Just "/etc/mail.conf", B.text = S.fromList "send = on\nreceive = off\n"}
            ],
        wrapping = Nothing
        }

loop editor = do
    update (vt editor) (picture editor)
    event <- next_event (vt editor)
    case event of

        EvKey key modifiers -> do
            let editor' = editor { 
                notification = Nothing,
                buffers = if C.isEmpty (buffers editor) 
                    then C.singleton B.empty 
                    else buffers editor
                }
            let pressed modifier = modifier `elem` modifiers
            case (key, pressed MCtrl, pressed MAlt || pressed MMeta, pressed MShift) of

                (KASCII 'q', True, False, False) -> do
                    shutdown (vt editor')

                (KASCII 'w', False, True, False) | wrapping editor' == Nothing -> do
                    loop editor' {
                        wrapping = Just False,
                        notification = Just (Right " Wrapping by character (not implemented)") 
                        }

                (KASCII 'w', False, True, False) | wrapping editor' == Just False -> do
                    loop editor' {
                        wrapping = Just True,
                        notification = Just (Right " Wrapping by word (not implemented)")
                        }

                (KASCII 'w', False, True, False) | wrapping editor' == Just True -> do
                    loop editor' {
                        wrapping = Nothing,
                        notification = Just (Right " Wrapping disabled") 
                        }

                (KASCII 'c', False, True, False) -> do
                    loop editor' { 
                        notification = Just (Left (" Success (2 warnings)", " Ctrl+M "))
                        }

                (KASCII 'd', True, False, False) -> loop (B.delete `active` editor')
                (KDel, False, False, False) -> loop (B.delete `active` editor')
                (KBS, False, False, False) -> loop (B.deleteLeft `active` editor')

                (KHome, all, False, False) -> loop (B.moveHome all `active` editor')
                (KEnd, all, False, False) -> loop (B.moveEnd all `active` editor')

                (KLeft, False, False, False) -> loop (B.moveLeft `active` editor')
                (KRight, False, False, False) -> loop (B.moveRight `active` editor')
                (KUp, False, False, False) -> loop (B.moveUp `active` editor')
                (KDown, False, False, False) -> loop (B.moveDown `active` editor')

                (KASCII 'u', all, True, False) -> loop (B.moveHome all `active` editor')
                (KASCII 'o', all, True, False) -> loop (B.moveEnd all `active` editor')

                (KASCII 'j', False, True, False) -> loop (B.moveLeft `active` editor')
                (KASCII 'l', False, True, False) -> loop (B.moveRight `active` editor')
                (KASCII 'i', False, True, False) -> loop (B.moveUp `active` editor')
                (KASCII 'k', False, True, False) -> loop (B.moveDown `active` editor')

                (KASCII 'n', True, False, False) -> loop editor' {
                    buffers = C.insertL B.empty (buffers editor'),
                    notification = Just (Left (" New file created", " Alt+, and Alt+. to switch "))
                    }
                    

                (KLeft, False, True, False) -> rotate C.rotL editor'
                (KRight, False, True, False) -> rotate C.rotR editor'
                
                (KLeft, False, True, True) -> rotate switchLeft editor'
                (KRight, False, True, True) -> rotate switchRight editor'

                (KASCII ',', False, True, False) -> rotate C.rotL editor'
                (KASCII '.', False, True, False) -> rotate C.rotR editor'

                (KASCII ';', False, True, _) -> rotate switchLeft editor'
                (KASCII ':', False, True, _) -> rotate switchRight editor'

                (KASCII '<', False, True, _) -> rotate switchLeft editor'
                (KASCII '>', False, True, _) -> rotate switchRight editor'

                (KASCII 'w', True, False, False) -> rotate C.removeR editor'

                (KASCII 's', True, False, False) -> case C.focus (buffers editor') of
                    Nothing -> loop editor'
                    Just b@B.Buffer { B.filename = Nothing } -> loop editor'
                    Just b@B.Buffer { B.filename = Just s } -> do
                        writeFile s (toList (B.text b))
                        loop editor' {
                            notification = Just (Right " File saved")
                            }
                    
                (KASCII '\t', False, False, False) -> loop (B.tab `active` editor')

                (KASCII c, False, False, _) -> loop (B.insert c `active` editor')
                (KEnter, False, False, False) -> do
                    let line = case C.focus (buffers editor') of
                            Nothing -> ""
                            Just b -> B.currentLine b
                    let indent = takeWhile (\c -> c == '\t' || c == ' ') line
                    loop (B.inserts ('\n':indent) `active` editor')

                _ -> do
                    loop editor'

            where
                switchLeft bs = case C.focus bs of
                    Just b -> C.insertR b (C.removeL bs)
                    Nothing -> bs
                switchRight bs = case C.focus bs of
                    Just b -> C.insertL b (C.removeR bs)
                    Nothing -> bs
                rotate f editor' = do
                    let buffers' = f (buffers editor')
                    loop editor' { 
                        buffers = buffers',
                        notification = Just (Right (" " ++ intercalate "  "
                            (mark $ map base $ C.rightElements $ buffers')))
                    }
                mark [] = []
                mark (x:xs) = ("[" ++ x ++ "]") : xs
                base :: B.Buffer -> String
                base b = case B.filename b of
                    Nothing -> "(unsaved file)"
                    Just s -> filename s
                filename s = tail $ toList $ S.takeWhileR (/= '/') $ S.fromList s

        EvResize width height -> do
            loop editor { 
                width = width, 
                height = height 
                }

        _ -> do
            loop editor
picture editor = case C.focus (buffers editor) of
    Nothing -> picture'
    Just buffer -> picture' { 
        pic_cursor = Cursor (fromIntegral x) (fromIntegral y)
        }
        where
            (x, y) = (B.column cursor - B.column scroll, B.line cursor - B.line scroll)
            scroll = B.scroll buffer
            cursor = B.cursor buffer
    where
        picture' = pic_for_image (draw editor)

positionString editor = case C.focus (buffers editor) of
    Nothing -> " 1:1 "
    Just buffer -> 
        " " ++ show (B.line (B.cursor buffer)) ++ ":" ++ 
        show (B.column (B.cursor buffer)) ++ " "

filenameString editor = case C.focus (buffers editor) of
    Nothing -> " (no file)"
    Just buffer -> case B.filename buffer of
        Nothing -> " (unsaved file)"
        Just filename -> " " ++ filename

draw editor =
        drawText (fromMaybe B.empty $ C.focus $ buffers editor) (width editor) (height editor - 1) 
    <->
        drawStatus editor

drawText buffer width height =
    vert_cat $ take height $ map (string textStyle . fit) (B.lines buffer)
    where
        fit s = take width s ++ replicate (max 0 (width - length s)) ' '

drawStatus editor@Editor { notification = Just (Left (l, r)) } = string statusStyle $ 
    splitLayout (width editor) l r
drawStatus editor@Editor { notification = Just (Right s) } = string statusStyle $ 
    take (width editor) s
drawStatus editor = string statusStyle $
    splitLayout (width editor) 
        (filenameString editor)
        (positionString editor)

splitLayout width left right = left' ++ right'
    where
        space = max 0 (width - length right')
        left' = take space (reverse (take space (reverse left)) ++ [' ', ' ' ..])
        right' = take width right

statusStyle = Attr {
    attr_style = SetTo default_style_mask,
    attr_fore_color = SetTo black,
    attr_back_color = SetTo white
    }

textStyle = Attr {
    attr_style = SetTo default_style_mask,
    attr_fore_color = SetTo white,
    attr_back_color = SetTo black
    }

