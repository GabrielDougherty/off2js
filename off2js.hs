{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as B
import Data.List.Split (splitOn)

gabestuff = [[0,0]
            ,[0,1]
            ,[3,4]
            ,[5,8]]

get2d lst index = [x |
                   (i,x) <- zip [0..] lst,
                   i == index]

makePolygon allPoints [] = []
makePolygon allPoints (x:xs) =
  get2d allPoints x ++ makePolygon allPoints xs

-- make triangles from polygon using "fan out" approach
makeTriangles [a,b,c] = [a,b,c]
makeTriangles (x:xs) = (x:nextTwo) ++ (makeTriangles $ x:afterTwo)
  where nextTwo = take 2 xs
        afterTwo = drop 1 xs

makeFileLines input = B.split '\n' input

-- filteredLines [] = []
filteredLines :: [B.ByteString] -> [B.ByteString]
filteredLines lines =
  foldl (\ lst elem ->
            if (B.head elem) == '#'
            then lst
            else lst ++ [elem]) [] lines

glueLines lines = B.intercalate "\n" lines

fileSansComments input = glueLines . filteredLines . makeFileLines $ input

-- https://stackoverflow.com/questions/8366093/how-do-i-parse-a-matrix-of-integers-in-haskell
sint = C.skipSpace >> int
int = liftM floor C.scientific
sdbl = C.skipSpace >>= return C.double

parsePoints :: C.Parser ([[Double]], [[Double]])
parsePoints = do
  str <- "OFF"
--  C.skipSpace
  m <- sint
  n <- sint
  garbage <- sint
  points <- C.count m (C.count n sdbl)
  let readPoly = do
        sides <- sint
        res <- C.count sides sdbl
        return res
  -- sides <- C.count m (readPoly)
  let sides = [[2]]
  return (points,sides)

parseIt :: IO ([[Double]],[[Double]])
parseIt = do
  contents <- B.getContents
  print contents
  let filtered = fileSansComments contents
  print "filtered"
  print filtered
  let res = C.parseOnly parsePoints filtered
  either (error . show) return res

main = do
  res <- parseIt
  let makeThese p = makePolygon (fst res) p
  let polygons = map (\p -> makeThese p) (snd res)
  mapM_ print polygons
  let triangles = concat . makeTriangles $ polygons
  -- mapM_ print triangles
  print "done"

