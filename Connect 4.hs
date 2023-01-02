import Data.List
type Player = Int
type Board = [[Integer]]

createRow n = map (\x -> (0, (n, x))) [0..6]
createEmptyBoard = map (\x -> (createRow x)) [0..5]

emptyBoard = replicate 6 (replicate 7 0)
allCords = [(x, y) | x<- [0..6], y<-[0..5]]
--testBoard = [[0,0,0,0,0,0,0],[0,0,0,0,0,1,0],[0,0,0,0,1,0,0],[0,0,0,1,0,0,0],[0,0,1,0,0,0,0],[0,0,0,0,0,0,0]]

drawBoardH [] = []
drawBoardH (x:xs)
 | x == 0 = " _" ++ drawBoardH xs
 | x == 1 = " x" ++ drawBoardH xs
 | otherwise = " o" ++ drawBoardH xs

drawBoard2 = map (\x -> drawBoardH x)
drawBoard1 board = reverse (drawBoard2 board)

stuffer :: String -> [String] -> String
stuffer x [] = []
stuffer x [y] = y
stuffer a (b:xs) = b ++ a ++ (stuffer a xs)

drawBoard board = stuffer "\n" (drawBoard1 board)

goForever :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
goForever (a, b) (c, d) = zip [a, a + c..] [b, b + d..]
goFour :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
goFour x y = take 4 (goForever x y)
allDirect = [(x, y) | x<-[-1..1], y<-[-1..1], (x, y) /= (0, 0)]
allFours (a, b) = map (\(x, y) -> goFour (a, b) (x, y)) allDirect
score1 player pieces
 | 1 `elem` pieces && 2`elem` pieces = 0
 | otherwise = 10 ^ (sum [ 1 | x <- pieces, x == player])

isWon1H board [] = []
isWon1H board (x:xs) = [[board!!a!!b| (a, b) <- x, a>=0, a<=5, b>=0, b<=6]] ++ isWon1H board xs

isWon1HH :: [[Integer]] -> [[[Integer]]]
isWon1HH board = map (\x -> isWon1H board (allFours x)) allCords

check1 bruh = map (\x -> score1 1 x) bruh
check1H bruh = map (\x -> check1 x) bruh

check2 bruh = map (\x -> score1 2 x) bruh
check2H bruh = map (\x -> check2 x) bruh

isWonC1 board = map (\x -> 10000 `elem` x) (check1H (isWon1HH board))
isWonC1H board = True `elem` isWonC1 board
isWonC2 board = map (\x -> 10000 `elem` x) (check2H (isWon1HH board))
isWonC2H board = True `elem` isWonC2 board

isWon board
 | isWonC1H board = True
 | isWonC2H board = True
 | otherwise = False

sfy board col = zip [0..5] (map (\x -> board!!x!!col) [0..5])
sfyH board col = head [a | (a, b) <- sfy board col, b == 0]

isMoveLegal board col = col <= 6 && col >= 0 && sfyH board col <= 5

replaceAtIndex n x xs = (take n xs) ++ [x] ++ (drop (n+1) xs)

makeMoveH board col player = (take col (board!!row)) ++ [player] ++ drop (col+1) (board!!row)
  where row = sfyH board col

makeMove board col player = replaceAtIndex row (makeMoveH board col player) board
  where row = sfyH board col

next_ player
 | player == 1 = 2
 | otherwise = 1

main = do
  putStrLn ("Welcome to Connect 4")
  let board = emptyBoard
  event_loop board 1

event_loop board player = do
  putStrLn $ drawBoard board
  print([0..6])

  if isWon board then do
    win_message board player
    return ()
  else do
    col <- getMove player
    handle_move board player col

handle_move board player col
    | col == -99            = goodbye
    | isMoveLegal board col = event_loop new_board (next_ player)
    | otherwise = complain_and_restart
    where complain_and_restart = do
              putStrLn "ERROR: That is not a legal move."
              event_loop board player
          new_board = makeMove board col player
          goodbye = do putStrLn "You quit"

getMove player = do
  putStrLn $ "(Enter -99 to quit.)"
  putStrLn $ "Player " ++(show player)++" moves."
  putStr $ "Column [0-6]? "
  x <- getLine
  return (get_number x)

get_number :: String -> Int
get_number colIn
    = case (reads colIn)::[(Int,String)] of
        [(colnum, "")] -> colnum
        _              -> -1

win_message board player = do
    putStrLn $ "The game is over"
    putStrLn $ "Player "++(show $ next_ player)++" won!"
