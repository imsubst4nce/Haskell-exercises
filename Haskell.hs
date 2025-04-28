----------------------------
--   SEIRA ASKISEWN #1    --
----------------------------
-- KOUTSONIKOLIS NIKOLAOS --
--        A.M: 5108       --
----------------------------

-- ASKHSH 1 --

cost :: (Int,Int,Int)->(Int,Int,Int)->Float
cost (h1, m1, s1) (h2, m2, s2)
    | not (validTime h1 m1 s1) || not (validTime h2 m2 s2) = error "Error: Provided an invalid time"
    | adjustedSeconds == 0 = 0.0  -- Midenikos xronos klisis
    | adjustedSeconds <= 180 = 0.58  -- Xronos klisis ews 3 lepta
    | otherwise = fromIntegral ( round ( 1000 * (0.58 + 0.003 * fromIntegral (adjustedSeconds - 180)) ) ) / 1000.0 -- Vasiko kostos + 0.003 to defterolepto apo ekei kai pera
    where
        validTime h m s = h >= 0 && h < 24 && m >= 0 && m < 60 && s >= 0 && s < 60 -- Akolouthoume to 24wro format
        seconds1 = h1 * 3600 + m1 * 60 + s1 -- Metatroph se defterolepta
        seconds2 = h2 * 3600 + m2 * 60 + s2 -- Metatroph se defterolepta
        adjustedSeconds =
            if seconds2 < seconds1
            then (24 * 3600 - seconds1) + seconds2 -- Allagi hmeras
            else seconds2 - seconds1

-----------------------------------------------------------------------------------------

-- ASKHSH 2 --

compress :: Integer->Integer
compress n
    | n < 0 = error "Error: Provided a negative number"
    | n < 10 = n -- Hdh monopsifios
    | otherwise = compress (productOfDigits n) -- Anadromiki klisi me ton arithmo
    where
        productOfDigits number
            | number == 0 = 1
            | digit /= 0 = digit * productOfDigits (number `div` 10)
            | otherwise = productOfDigits (number `div` 10)
            where digit = number `mod` 10

-----------------------------------------------------------------------------------------

-- ASKHSH 3 --

search :: Integer->Integer->Integer->Integer
search a k m
    | a < 0 || k < 0 || m < 2 = error "Error: Provided a negative number or m < 2"
    | otherwise = searchForSmallestPositiveInteger 1 -- Anadromiki klisi ksekinontas apo to 1, giati eimai sto N*
    where
        searchForSmallestPositiveInteger n
            | (n + a)^k < m^n = n -- An mpoume edw vrikame ton mikrotero n pou psaxnoume
            | otherwise = searchForSmallestPositiveInteger (n + 1) -- Alliws ksanakaloume me ton epomeno n

-----------------------------------------------------------------------------------------

-- ASKHSH 4 --

sum2025 :: Integer->Integer->Integer
sum2025 m n
    | n < 0 || m < 0 = error "Error: Provided a negative number"
    | otherwise = calculateSum m n m -- Pernaw dyo fores to m stin mia xrisimopoieitai san to i sto athroisma
    where
        calculateSum m n i
            | n < i = 0 -- Den thelw na ypervw to n
            | otherwise = (n+i)^m + calculateSum m n (i+1) -- Anadromiki klisi me afksisi tou i

-----------------------------------------------------------------------------------------

-- ASKHSH 5 --

statistics :: [(Int,Int)]->(Int,Int,Int,Int,Int)
statistics = calculateStatistics
    where
        calculateStatistics [] = (0,0,0,0,0)
        calculateStatistics ((goals_put, goals_received): pair) =
            let (matches, total_points, total_goals_put, total_goals_received, best_diff) = calculateStatistics pair
                current_diff = goals_put - goals_received
                new_points
                    | goals_put > goals_received = 3  -- niki -> 3 pontoi
                    | goals_put == goals_received = 1 -- isopalia -> 1 pontos
                    | otherwise = 0                   -- htta -> 0 pontoi
                new_best_diff
                    | matches == 0 = current_diff                              -- prwtos agwnas
                    | goals_put > goals_received = max current_diff best_diff  -- niki
                    | goals_put == goals_received && best_diff > 0 = best_diff -- isopalia me proigoumeni niki, kratame niki
                    | goals_put == goals_received = 0                          -- isopalia xwris proigoumeni niki, gyrname miden
                    | best_diff >= 0 = best_diff                               -- an yparxei niki h isopalia kratame afto
                    | abs current_diff < abs best_diff = current_diff         -- htta me kalyteri diafora
                    | otherwise = best_diff                                   -- htta me xeiroteri diafora
            in (
                matches + 1,
                total_points + new_points,
                total_goals_put + goals_put,
                total_goals_received + goals_received,
                new_best_diff
            )

-----------------------------------------------------------------------------------------

-- ASKHSH 6 --

wordList :: String->[String]
wordList s
    | s == "" = [] -- An einai keno string
    | otherwise = parseString s ""
    where
        parseString [] accumulator
            | accumulator == "" = []
            | otherwise = [accumulator]
        parseString (character:substring) accumulator
            | isLatin character = parseString substring (accumulator ++ [character])
            | accumulator == "" = parseString substring ""
            | otherwise = accumulator : parseString substring ""
        isLatin c = isAsciiLower c || isAsciiUpper c

-- Voithitikes sinartiseis elegxou mikrwn kai megalwn xarakthrwn
isAsciiLower :: Char -> Bool
isAsciiLower c
    | c >= 'a' && c <= 'z' = True
    | otherwise = False

isAsciiUpper :: Char -> Bool
isAsciiUpper c
    | c >= 'A' && c <= 'Z' = True
    | otherwise = False

-----------------------------------------------------------------------------------------

-- ASKHSH 7

size :: [[t]] -> (Int,Int)

size s
    | null s || null (head s) || not (all (\x -> length x == length (head s)) s) = (0,0)
    | otherwise = (length s, length (head s))

transpose :: [[u]]->[[u]]
transpose t
    | size t == (0,0) = []
    | otherwise = transposeHelper t
    where
        transposeHelper [] = []
        transposeHelper matrix@(x:_)
            | null x = []
            | otherwise = map head matrix : transposeHelper (map tail matrix)

matrixmult :: [[Int]]->[[Int]]->[[Int]]
matrixmult a b
    | size a == (0,0) || size b == (0,0) = [] -- mi egkyroi pinakes
    | cols_a /= rows_b = [] -- den ginetai pol/smos pinakwn
    | otherwise = matrixmultHelper a b
    where
        (rows_a, cols_a) = size a
        (rows_b, cols_b) = size b
        matrixmultHelper a b = [[sum (zipWith (*) row col) | col <- transpose b] | row <- a]

-----------------------------------------------------------------------------------------

-- ASKHSH 8 --

trace :: [(Int,Int)]->[(Int,Int)]
trace s
    | null s || s == [(0,0)] = error "Error: Provided an empty instruction list"
    | otherwise = removeDuplicates (concat [createPath p1 p2 | (p1, p2) <- zip fullPath (tail fullPath)])
    where
        -- Arxika kai telika simeia
        fullPath = (0,0) : s ++ [(0,0)]

        -- Afairesi diplwtipwn apo tin lista
        removeDuplicates [] = []
        removeDuplicates [x] = [x]
        removeDuplicates (x:y:xs)
            | x == y = removeDuplicates (y:xs)
            | otherwise = x : removeDuplicates (y:xs)

        createPath (x1,y1) (x2,y2)
            | (x1,y1) == (x2,y2) = [(x1,y1)]
            | otherwise =
                let dx = x2 - x1
                    dy = y2 - y1
                    diagonalSteps = min (abs dx) (abs dy)
                    signX = signum dx
                    signY = signum dy

                    -- Diagwnia kinisi
                    diagonalPath = [(x1 + i*signX, y1 + i*signY) | i <- [0..diagonalSteps]]

                    -- Thesi meta apo diagwnia kinisi
                    (xAfter, yAfter) = (x1 + diagonalSteps*signX, y1 + diagonalSteps*signY)

                    -- Eftheia kinisi
                    straightPath = if xAfter == x2
                                  then [(xAfter, yAfter + i*signY) | i <- [1..(abs dy - diagonalSteps)]]
                                  else [(xAfter + i*signX, yAfter) | i <- [1..(abs dx - diagonalSteps)]]
                in diagonalPath ++ straightPath

-----------------------------------------------------------------------------------------

-- ASKHSH 9 --

generating :: (Int->Double)->Int->(Double->Double)
generating f k
    | k < 0 = error "Error: Provided a negative number"
    | otherwise = \z -> fromIntegral( round ( sum ([f i * z^i | i <- [0..k]]) * 10^5 ) ) / 10.0^5

-----------------------------------------------------------------------------------------

-- ASKHSH 10 --

apply :: Ord u => [v->u]->[v]->[u]
apply q s = removeDuplicates (sort [f x | f <- q, x <- s])
  where
    sort [] = []
    sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]
    removeDuplicates [] = []
    removeDuplicates [x] = [x]
    removeDuplicates (x:y:xs)
      | x == y = removeDuplicates (y:xs)
      | otherwise = x : removeDuplicates (y:xs)