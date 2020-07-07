module MazePainter where

import Control.Monad.State

import Data.Array
import Data.List

--------------------------------------------------------
-- test case and utilities to convert and display it: --
--------------------------------------------------------

maze24 = [
  "##################################",
  "#       #     #    #  #  #    #  #",
  "#  #       #  #    #  #  #    #  #",
  "############# ####################",
  "#  #    #  #          #       #  #",
  "#  #    #  #  #    #     #       #",
  "################################ #",
  "#  #    #  #  #    #  #  #    #  #",
  "#  #    #  #  #    #  #  #    #  #",
  "################################ #",
  "#  #    #  #  #       #  #       #",
  "#                  #          #  #", -- ( 16, 11)
  "# ################################",
  "#  #    #  #  #    #  #  #    #  #",
  "#  #    #  #  #    #  #  #    #  #",
  "# ################################",
  "#  #       #  #       #          #",
  "#       #          #     #    #  #",
  "################################ #",
  "#  #    #  #  #    #  #  #    #  #",
  "#  #    #  #  #    #  #  #    #  #",
  "############################### ##",
  "#       #     #    #  #  #    #  #",
  "#  #       #  #    #  #  #    #  #",
  "############ ################## ##",
  "#  #    #  #       #          #  #",
  "#  #    #  #  #       #  #       #",
  "##################################"]

maze11 = [
  "############      ",
  "#  #    #  #      ",
  "#  #    #  #      ",
  "######### ##      ",
  "#  #       #      ",
  "#  #    #  #      ",
  "## # ## # ##      ",
  "#  #       #      ",
  "#       #  #      ",
  "############      "]

type MazeT = Array (Int, Int) Char

toArray2d :: [[Char]] -> MazeT
toArray2d maze =
    listArray ((1,1), ((length maze), length (head maze))) (concat maze)

maze11arr = toArray2d maze11
maze24arr = toArray2d maze24

m11a = maze11arr
m24a = maze24arr

-- printMaze --------------

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice i s = (take i s):(slice i (drop i s))

showMaze :: MazeT -> String
showMaze m =
    intercalate "\n" $
    let boundsM = bounds m in
    slice ((snd (snd boundsM)) - (snd (fst boundsM)) + 1) (elems m)

printMaze :: MazeT -> IO ()
printMaze m = putStrLn $ showMaze m

---------------------------------------------------------------
-- END OF: test case and utilities to convert and display it --
---------------------------------------------------------------


--------------------------------------------------------
--       paint :: (Int, Int) -> State MazeT ()        --
--------------------------------------------------------

floorFacet = ' '
startColor = '+'

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (a, b) (c, d) = (a + c, b + d)


paintInPlace :: (Int, Int) -> Char -> State MazeT ()
paintInPlace coords color = modify $
  \maze -> if maze!coords == floorFacet then maze//[(coords, color)] else maze

paintInDirection1 :: (Int, Int) ->  Char -> (Int, Int) -> MazeT -> MazeT
paintInDirection1 coords color vector maze =
  if maze!(plus coords vector) == floorFacet
      then snd $ runState (paint1 (plus coords vector) color) maze else maze

paintInDirection :: (Int, Int) -> Char -> (Int, Int) -> State MazeT ()
paintInDirection coords color vector =
  modify (paintInDirection1 coords color vector)



paint1 :: (Int, Int) -> Char -> State MazeT [()]
paint1 vector color = do
    paintInPlace vector color
    forM [(0, 1), (0, -1), (-1, 0), (1, 0)] (paintInDirection vector color)

paint0 :: (Int, Int) -> Char -> MazeT -> MazeT
paint0 vector color maze = snd $ runState (paint1 vector color) maze

paint :: (Int, Int) -> Char -> MazeT -> MazeT
paint v c m = (paint0 v c m) // [(v, '%')]

--------------------------------------------------------
--                         END                        --
--------------------------------------------------------

------------
-- tests: --
------------

om11a = paint ( 5,  7) '.' m11a
om24a = paint (12, 17) '~' m24a

----------------
-- use cases: --
----------------

{-

*MazePainter> printMaze om11a
############      
#  #    #..#      
#  #    #..#      
#########.##      
#..#..%....#      
#..#....#..#      
##.#.##.#.##      
#..#.......#      
#.......#..#      
############      
*MazePainter> printMaze om24a
##################################
#~~~~~~~#~~~~~#    #  #  #    #  #
#~~#~~~~~~~#~~#    #  #  #    #  #
#############~####################
#  #    #  #~~~~~~~~~~#~~~~~~~#~~#
#  #    #  #~~#~~~~#~~~~~#~~~~~~~#
################################~#
#  #    #  #  #    #  #  #    #~~#
#  #    #  #  #    #  #  #    #~~#
################################~#
#~~#~~~~#~~#~~#~~~~~~~#~~#~~~~~~~#
#~~~~~~~~~~~~~~~%~~#~~~~~~~~~~#~~#
#~################################
#~~#    #  #  #    #  #  #    #  #
#~~#    #  #  #    #  #  #    #  #
#~################################
#~~#~~~~~~~#~~#~~~~~~~#~~~~~~~~~~#
#~~~~~~~#~~~~~~~~~~#~~~~~#~~~~#~~#
################################~#
#  #    #  #  #    #  #  #    #~~#
#  #    #  #  #    #  #  #    #~~#
###############################~##
#~~~~~~~#~~~~~#    #  #  #    #~~#
#~~#~~~~~~~#~~#    #  #  #    #~~#
############~##################~##
#  #    #  #~~~~~~~#~~~~~~~~~~#~~#
#  #    #  #~~#~~~~~~~#~~#~~~~~~~#
##################################

-}