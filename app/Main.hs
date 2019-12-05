import System.Exit
import Text.Read
import System.Environment

exit :: IO ()
exit = putStrLn "Error\n" >> exitWith (ExitFailure 84)

removeAllChars :: Eq a => [a] -> [a] -> [a]
removeAllChars = filter . flip notElem

getElemAtIndex :: [a] -> Int -> a
getElemAtIndex list index = head (drop index list)

removeAllBeforeNb :: String -> String
removeAllBeforeNb = tail . dropWhile (/=',')

getUnparsedArray :: Int -> String -> [String]
getUnparsedArray n str = take n $ iterate removeAllBeforeNb str

getValueArray :: Int -> String -> [String]
getValueArray n str = map (takeWhile (/= ',')) (getUnparsedArray n (removeAllChars "()" str))

getInt :: String -> Float
getInt str = read str :: Float

parseTupple :: Int -> String -> [Float]
parseTupple n str = map getInt (getValueArray n str)

data Color = Color Float Float Float
instance Show Color where
    show (Color r g b) = "(" ++ (takeWhile (/= '.') (show r)) ++ "," ++ (takeWhile (/= '.') (show g)) ++ "," ++ (takeWhile (/= '.') (show b)) ++ ")"

data Position = Position Float Float
instance Show Position where
    show (Position x y) = "(" ++ (takeWhile (/= '.') (show x)) ++ "," ++ (takeWhile (/= '.') (show y)) ++ ")"

data Pixel = Pixel Position Color
instance Show Pixel where
    show (Pixel pos color) = (show pos) ++ " " ++ (show color)  ++ "\n"

data Cluster = Cluster Color Color [Pixel]
instance Show Cluster where
    show (Cluster old new list) = "--\n" ++ showClusterColor new ++ "\n-\n"

showClusterColor :: Color -> [Char]
showClusterColor (Color r g b) = "(" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show b) ++ ")"

getColor :: String -> Color
getColor str = Color (head tab) (head (tail tab)) (last tab)
    where tab = parseTupple 3 str

getPosition :: String -> Position
getPosition str = Position (head tab) (last tab)
    where tab = parseTupple 2 str

addPixel :: String -> Pixel
addPixel line = Pixel (getPosition (head (tab))) (getColor (last (tab)))
    where
        tab = words (removeAllChars "()" line)

createPixelTab :: [String] -> [Pixel]
createPixelTab tab = map addPixel tab

getDistance :: Color -> Color -> Float
getDistance (Color xa ya za) (Color xb yb zb) = sqrt ((xa - xb)^2 + (ya - yb)^2 + (za - zb)^2)

findValueInTab :: [Pixel] -> Int -> Int -> Pixel
findValueInTab tab k n = getElemAtIndex tab ((((length tab) * n) `div` k) - 1)

getPixelColor :: Pixel -> Color
getPixelColor (Pixel pos color) = color

getColorR :: Color -> Float
getColorR (Color r g b) = r

getColorG :: Color -> Float
getColorG (Color r g b) = g

getColorB :: Color -> Float
getColorB (Color r g b) = b

getNewAverageColor :: Cluster -> Color
getNewAverageColor (Cluster old new list) = new

getPixelList :: Cluster -> [Pixel]
getPixelList (Cluster old new list) = list

createCluster :: [Pixel] -> Int -> Int -> Cluster
createCluster tab k n = Cluster (Color 0 0 0) (getPixelColor (findValueInTab tab k n)) []

findRandomClusters :: [Pixel] -> Int -> [Cluster]
findRandomClusters tab k= map (createCluster tab k) [1..k]

checkSpaces :: String -> Bool
checkSpaces str = if length (words str) == 2 then True else False

checkCommas :: String -> Int -> Bool
checkCommas str n = if length (filter (== ',') str) == n then True else False

checkLine :: String -> Bool
checkLine file
    | checkSpaces file == False = False
    | checkCommas (head (words file)) 1 == False = False
    | checkCommas (last (words file)) 2 == False = False
    | otherwise = True

parseFile :: String -> Int -> IO ()
parseFile file k = do
    content <- readFile file
    -- ERROR HANDLING -> MISSING NUMBER OR NOT NUMBER (1,) OR (1,e)
    print (map (checkLine) (lines content))
    -- if and (map (checkLine) (lines content)) then startAlgorithm (createPixelTab (lines content)) k else exit

getAllDistances :: [Cluster] -> Pixel -> [Float]
getAllDistances clusterTab pixel = map (getDistance (getPixelColor pixel)) (map getNewAverageColor clusterTab)

findSmallest :: [Float] -> Float
findSmallest tab = foldr1 min tab

getIndexOf :: Float -> [Float] -> Int -> Int
getIndexOf goal list index
    | getElemAtIndex list index == goal = index
    | otherwise = getIndexOf goal list (index + 1)

addToPixelList :: Cluster -> Pixel -> Cluster
addToPixelList (Cluster old new list) pixel = Cluster old new (list ++ [pixel])

updateClusterList :: [Cluster] -> Pixel -> [Cluster]
updateClusterList clusterTab pixel = take n clusterTab ++ [cluster] ++ drop (n + 1) clusterTab
    where
        distTab = getAllDistances clusterTab pixel
        n = getIndexOf (findSmallest distTab) distTab 0
        cluster = addToPixelList (getElemAtIndex clusterTab n) pixel

fillPixelList :: [Pixel] -> [Cluster] -> [Cluster]
fillPixelList pixelTab [] = []
fillPixelList [] clusterTab = clusterTab
fillPixelList pixelTab clusterTab = fillPixelList (tail pixelTab) (updateClusterList clusterTab (head pixelTab))


startAlgorithm :: [Pixel] -> Int -> IO ()
startAlgorithm [] k = exit
startAlgorithm pixelTab k = printClusters (loop pixelTab [] (findRandomClusters pixelTab k))

calculateAverage :: [Float] -> Float
calculateAverage tab = (foldl (+) 0 tab) / (fromIntegral (length tab))

getColorAverage :: [Color] -> Color
getColorAverage colorTab = Color (calculateAverage (map (getColorR) colorTab)) (calculateAverage (map (getColorG) colorTab)) (calculateAverage (map (getColorB) colorTab))

updateClusterColors :: Cluster -> Cluster
updateClusterColors (Cluster old new list) = (Cluster new (getColorAverage (map getPixelColor list)) list)

compareClustersColors :: Cluster -> Bool
compareClustersColors (Cluster old new list)
    | getColorR old /= getColorR new = False
    | getColorG old /= getColorG new = False
    | getColorB old /= getColorB new = False
    | otherwise = True

filterClusters :: [Cluster] -> Bool -> [Cluster]
filterClusters [] ok = []
filterClusters clusterTab ok = filter (\cluster -> compareClustersColors cluster == ok) clusterTab

clearClusterList :: Cluster -> Cluster
clearClusterList (Cluster old new list) = (Cluster old new [])

loop :: [Pixel] -> [Cluster] -> [Cluster] -> [Cluster]
loop pixelTab finishedClusters [] = finishedClusters
loop pixelTab finishedClusters clusterTab = loop pixelTab (finishedClusters ++ (filterClusters clusterTab True)) (map updateClusterColors (fillPixelList pixelTab (map clearClusterList (filterClusters clusterTab False))))

printClusterList :: Cluster -> [Char]
printClusterList (Cluster old new list) = concatMap show list

printClusters :: [Cluster] -> IO ()
printClusters clusterTab = putStrLn (concatMap (\cluster -> show cluster ++ (printClusterList cluster)) clusterTab)

start :: [String] -> IO ()
-- ERROR HANDLING
-- HANDLING -> k <= 1
start [] = exit
start [k, e, path] = parseFile path (read k :: Int)
start tab = exit

main :: IO ()
main = start =<< getArgs
