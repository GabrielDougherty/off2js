{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Text.Printf

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
            if (B.length elem) == 0 || (B.head elem) == '#'
            then lst
            else lst ++ [elem]) [] lines

glueLines lines = B.intercalate "\n" lines

fileSansComments input = glueLines . filteredLines . makeFileLines $ input

-- https://stackoverflow.com/questions/8366093/how-do-i-parse-a-matrix-of-integers-in-haskell
sint = C.skipSpace >> int
int = liftM floor C.scientific
sdbl = C.skipSpace >> liftM id C.double

skipRestOfLine = C.skipWhile (\c -> c /= '\n' && c /= '\r')

readPoly = do
  sidePoints <- sint
  res <- C.count sidePoints sint
  skipRestOfLine

  return res

parsePoints :: C.Parser ([[Double]], [[Int]])
parsePoints = do
  str <- "OFF"
  m <- sint
  n <- sint
  sint

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

strList :: [Double] -> String
strList lst = "[" ++ csv ++ "]"
  where
    csv = L.intercalate "," $ map showDigits lst
    showDigits = printf "%.9f"

str2dList :: [[Double]] -> String
str2dList lst = "[" ++ L.intercalate "," (map strList lst) ++ "]"

jscriptModel :: [[Double]] -> Int -> String -> String
jscriptModel triangles bcSize modelName = "function " ++ modelName ++
  "() {\n\tthis.triangles = " ++ str2dList triangles ++ ";" ++
  "\n\tthis.BC = " ++ makeBary bcSize  ++ ";" ++ "\n};"

scaleTriangles :: [[Double]] -> [[Double]]
scaleTriangles triangles = scaled
  where
    triMax = maximum $ map maximum triangles
    scaled = map (map (* (1/triMax))) triangles

-- shift a triangle list to all positive values
shiftPositive :: [[Double]] -> [[Double]]
shiftPositive triangles = shifted
  where
    offset triangle = map (\(i,p) -> p - triMin i) .
      zip [0..] $ triangle
    triMin coord =  minimum $ map (!!coord) triangles
    shifted = map offset triangles

-- shift a triangle list from 1,0 dimensions to 0.5,-0.5
shiftUnitCube :: [[Double]] -> [[Double]]
shiftUnitCube triangles = shifted
  where
    amt = -0.5
    shifted = map (map (+ amt)) triangles

fixTriangles triangle = shiftUnitCube . scaleTriangles .
  shiftPositive $ triangle

parseIt :: IO ([[Double]],[[Int]])
parseIt = do
  contents <- B.getContents
  let filtered = fileSansComments contents
  let res = C.parseOnly parsePoints filtered
  either (error . show) return res


modelFromArgs :: [String] -> String
modelFromArgs args =
  if length(args) == 2 && args!!0 == "-modelName"
  then args!!1
  else "CubeModel"

main = do
  progArgs <- getArgs
  let modelName = modelFromArgs progArgs

  (points,sides) <- parseIt
  let makeThese p = makePolygon points p
  let polygons = map (\p -> makeThese p) sides
  let triangles = concat $ map makeTriangles polygons
  let numTriangles = length(triangles) `div` 3
  let fixed = jscriptModel (fixTriangles triangles) numTriangles modelName
  putStrLn $ fixed

