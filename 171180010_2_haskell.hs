maxYap :: [Int] -> [Int]
maxYap [] = [] 
maxYap (x:xs) = inf : maxYap xs
inf = maxBound :: Int

falseYap :: [Int] -> [Bool]
falseYap [] =[] 
falseYap (x:xs) = False : falseYap xs

minBul :: [Int] -> [Bool] -> Int -> Int ->Int -> Int
minBul [] [] x y source = source
minBul distance shortestPath maxSayi minIdx source = 
            if source < 9
              then if ((distance!!source) <=maxSayi)  && ((shortestPath!!source)==False)
                         then minBul distance shortestPath (distance!!source) source (source+1)
                      else minBul distance shortestPath maxSayi minIdx (source+1)
              else minIdx

trueYap :: [Bool] -> Int ->[Bool]
trueYap yeni minIdx = case minIdx of 
                            0 -> [True,(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                            1 -> [(yeni!!0),True,(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                            2 -> [(yeni!!0),(yeni!!1),True,(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                            3 -> [(yeni!!0),(yeni!!1),(yeni!!2),True,(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                            4 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),True,(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                            5 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),True,(yeni!!6),(yeni!!7),(yeni!!8)]
                            6 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),True,(yeni!!7),(yeni!!8)]
                            7 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),True,(yeni!!8)]
                            8 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),True]
                            _ -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]

dijkstra_for1 :: [[Int]] -> [Int] -> [Bool] -> Int -> Int ->[Int] 
dijkstra_for1 [[]] [] [] minIdx i = []
dijkstra_for1 graph distance shortestPath minIdx i = do
  let minIdx =minBul distance shortestPath inf (1) (0)
  if i<8
       then dijkstra_for2 graph distance (trueYap shortestPath minIdx) minIdx (0) i
   else distance 

dijkstra_for2 ::  [[Int]] -> [Int] -> [Bool] -> Int ->  Int -> Int ->[Int] 
dijkstra_for2 [[]] [] [] (0) (0) (0) = []
dijkstra_for2 graph distance shortestPath minIdx v i = do
 if v<9
       then dijkstra_for2 graph (karsilastirma_islem graph distance shortestPath minIdx v) shortestPath minIdx (v+1) i 
    else dijkstra_for1 graph distance shortestPath minIdx (i+1)

karsilastirma_islem :: [[Int]] -> [Int] -> [Bool] -> Int ->  Int -> [Int]
karsilastirma_islem [[]] [] [] (0) (0) = []
karsilastirma_islem graph distance shortestPath minIdx v = do
 if (shortestPath!!v) == False && (distance!!minIdx)/=inf && ((distance!!minIdx) + (graph !! minIdx !! v)) < (distance!!v) && (graph !! minIdx !! v)/= 0 
     then toplamlar distance ((distance!!minIdx) + (graph !! minIdx !! v)) v
   else distance

toplamlar :: [Int] -> Int -> Int ->[Int]
toplamlar yeni toplam v = case v of 
                      0 -> [toplam,(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                      1 -> [(yeni!!0),toplam,(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                      2 -> [(yeni!!0),(yeni!!1),toplam,(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                      3 -> [(yeni!!0),(yeni!!1),(yeni!!2),toplam,(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                      4 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),toplam,(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
                      5 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),toplam,(yeni!!6),(yeni!!7),(yeni!!8)]
                      6 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),toplam,(yeni!!7),(yeni!!8)]
                      7 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),toplam,(yeni!!8)]
                      8 -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),toplam]
                      _ -> [(yeni!!0),(yeni!!1),(yeni!!2),(yeni!!3),(yeni!!4),(yeni!!5),(yeni!!6),(yeni!!7),(yeni!!8)]
main :: IO()
main = do
  let graph = [ [ 0,4,0,0,0,0,0,8,0],
          [ 8,0,7,0,0,0,0,0,0 ],
          [0,7,0,1,6,0,0,0,0],
          [ 0,0,1,0,5,0,0,11,0],
          [ 0,0,6,5,0,2,0,0,0],
          [0,0,0,0,2,0,3,0,0],
          [0,0,0,0,0,3,0,0,9],
          [8,0,0,11,0,0,0,0,10],
          [0,0,0,0,0,0,9,10,0 ]]
  let shortestPath= [False,False,False,False,False,False,False,False,False] 
  let infinity = maxBound :: Int
  let distance=[0,infinity,infinity,infinity,infinity,infinity,infinity,infinity,infinity]
  putStrLn "Graph:"
  print $ graph!!0
  print $ graph!!1
  print $ graph!!2
  print $ graph!!3
  print $ graph!!4
  print $ graph!!5
  print $ graph!!6
  print $ graph!!7
  print $ graph!!8
  putStrLn "   "
  putStrLn "Source Vertex:0"
  putStrLn "   "
  putStrLn "Distance from source vertex to other vertices:"
  let d=dijkstra_for1 graph distance shortestPath 0 0
  putStr "Vertex 0: "
  print $ d!!0
  putStr "Vertex 1: "
  print $ d!!1
  putStr "Vertex 2: "
  print $ d!!2
  putStr "Vertex 3: "
  print $ d!!3
  putStr "Vertex 4: "
  print $ d!!4
  putStr "Vertex 5: "
  print $ d!!5
  putStr "Vertex 6: "
  print $ d!!6
  putStr "Vertex 7: "
  print $ d!!7
  putStr "Vertex 8: "
  print $ d!!8
  --print $ dijkstra_for1 graph distance shortestPath 0 0
