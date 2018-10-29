{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L

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

makeBary :: Int -> String
makeBary numTriangles = "[" ++ csv ++ "]"
  where
    tri = "[1,0,0],[0,1,0],[0,0,1]"
    csv = L.intercalate "," $ replicate numTriangles tri

bstrList :: [Double] -> String
bstrList lst =  L.intercalate "," $ map show lst

strList :: [Double] -> String
strList lst = "[" ++ bstrList lst ++ "]"

str2dList :: [[Double]] -> String
str2dList lst = "[" ++ L.intercalate "," (map strList lst) ++ "]"

jscriptModel :: [[Double]] -> Int -> String -> String
jscriptModel triangles bcSize modelName = "function " ++ modelName ++
  "() {\n\tthis.triangles = " ++ str2dList triangles ++ ";" ++
  "\nthis.bc = " ++ makeBary bcSize  ++ ";" ++ "\n};"

scaleTriangles :: [[Double]] -> [[Double]]
scaleTriangles triangles = scaled
  where
    max = maximum $ map maximum triangles
    scaled = map (map (* max)) triangles

-- shift a triangle list to all positive values
shiftPositive :: [[Double]] -> [[Double]]
shiftPositive triangles = shifted
  where
    min = minimum $ map minimum triangles
    offset = if min < 0
            then -min
            else 0
    shifted = map (map (+ offset)) triangles

-- shift a triangle list from 1,0 dimensions to 0.5,-0.5
shiftUnitCube :: [[Double]] -> [[Double]]
shiftUnitCube triangles = shifted
  where
    amt = -0.5
    shifted = map (map (+ amt)) triangles

parseIt :: IO ([[Double]],[[Int]])
parseIt = do
  contents <- B.getContents
  let filtered = fileSansComments contents
  let res = C.parseOnly parsePoints filtered
  either (error . show) return res

main = do
  res <- parseIt
  let makeThese p = makePolygon (fst res) p
  let polygons = map (\p -> makeThese p) (snd res)
  let triangles = concat $ map makeTriangles polygons
  putStrLn $ jscriptModel triangles (length(triangles)) ("BobsBurgers"::String)

