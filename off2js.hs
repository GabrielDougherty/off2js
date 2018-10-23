gabestuff = [[0,0]
            ,[0,1]
            ,[3,4]
            ,[5,8]]

get2d lst index = [x |
                   (i,x) <- zip [0..] lst,
                   i == index]

makePolygon allPoints [] = []
makePolygon allPoints (x:xs) = get2d allPoints x ++ makePolygon allPoints xs

-- make triangles using "fan out" approach
makeTriangles [a,b,c] = [[a,b,c]]
makeTriangles (x:xs) = (x:nextTwo) : (makeTriangles $ x:afterTwo)
  where nextTwo = take 2 xs
        afterTwo = drop 1 xs

