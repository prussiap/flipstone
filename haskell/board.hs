module Main where

import Data.List (sort,delete)

newtype Scrap = Scrap [Board] deriving (Show)
newtype Cut = Cut Double deriving (Eq, Ord, Show)
data CutEvent = CutEvent (Cut, Board) deriving (Show)
type CutHistory = [CutEvent]
newtype Board = Board Double deriving (Eq, Ord, Show)

boardLength (Board length) = length
scrapBoards (Scrap boards) = boards

scrap :: Scrap
scrap = Scrap $ map Board
  [
    96, 96, 96, 96,
    48,
    30, 30
  ]

cuts :: [Cut]
cuts = map Cut [
  44.75, 44.75, 44.75,
  20.75, 20.75, 20.75, 20.75,
  34.25, 34.25, 34.25, 34.25
  ]

cutAllFromScrap :: Scrap -> [Cut] -> ([Board], Scrap, CutHistory)
cutAllFromScrap scrap cuts = foldl cutOne
                                   ([], scrap, [])
                                   (reverse . sort $ cuts)
  where cutOne (boards, scrap, history) cut =
            let (board, scrap', cutEvent) = cutFromScrap scrap cut
            in (board:boards, scrap', history ++ [cutEvent])

cutFromScrap :: Scrap -> Cut -> (Board, Scrap, CutEvent)
cutFromScrap scrap cut = cutBoard shortestScrap otherScrap cut
  where (shortestScrap, otherScrap) = takeShortedScrapForCut cut scrap

cutBoard :: Board -> Scrap -> Cut -> (Board, Scrap, CutEvent)
cutBoard board@(Board boardLength) scrap cut@(Cut cutLength) =
    (Board cutLength,
     putBack (Board remainingLength) scrap,
     CutEvent (cut, board))
  where remainingLength = boardLength - cutLength

takeShortedScrapForCut :: Cut -> Scrap -> (Board, Scrap)
takeShortedScrapForCut (Cut cutLength) (Scrap boards) =
   (shortest, Scrap (delete shortest boards))
  where (shortest:_) = sort (filter longerThanCut boards)
        longerThanCut = (>= cutLength) . boardLength

putBack :: Board -> Scrap -> Scrap
putBack board (Scrap boards) = Scrap (board:boards)

main = do
  let (boards, leftovers, history) = cutAllFromScrap scrap cuts
  putStrLn "Boards Cut"
  putStrLn "----------"
  mapM_ (putStrLn . show) boards

  putStrLn ""
  putStrLn "Scrap Leftover"
  putStrLn "--------------"
  mapM_ (putStrLn . show) (scrapBoards leftovers)

  putStrLn ""
  putStrLn "Cuts Made"
  putStrLn "---------"
  mapM_ (putStrLn . show) history

