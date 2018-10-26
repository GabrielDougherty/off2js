{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as B

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
makeTriangles :: [[Double]] -> [[Double]]
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

readPoly = do
  sidePoints <- sint
  res <- C.count sidePoints sint
  return res

parsePoints :: C.Parser ([[Double]], [[Int]])
parsePoints = do
  str <- "OFF"
  m <- sint
  n <- sint
  garbage <- sint

  let dimensions = 3
  points <- C.count m (C.count dimensions sdbl)
  sides <- C.count n (readPoly)

  return (points,sides)

myPrint x = (putStrLn . show) x

parseIt :: IO ([[Double]],[[Int]])
parseIt = do
  contents <- B.getContents
  myPrint contents
  let filtered = fileSansComments contents
  let res = C.parseOnly parsePoints filtered
  either (error . show) return res

main = do
  res <- parseIt
  let makeThese p = makePolygon (fst res) p
  let polygons = map (\p -> makeThese p) (snd res)
  mapM_ myPrint polygons
  let triangles = concat $ map makeTriangles polygons
  putStrLn "triangles:"
  mapM_ print triangles
  print "done"

