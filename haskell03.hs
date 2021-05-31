--Pratica 3 Haskell
--Nome: Robson Daniel Marchesan

add10toall :: [Int] -> [Int]
add10toall ad = [x+10 | x<-ad]

multN :: Int -> [Int] -> [Int]
multN n lst = [x*n | x <- lst]

multN' :: Int -> [Int] -> [Int]
multN' num lsta = map (\x -> num *x)lsta

applyExpr :: [Int] -> [Int]
applyExpr inp = [3*x+2 | x <- inp]

applyExpr' :: [Int] -> [Int]
applyExpr' inpu = map (\x -> 3*x+2)inpu

addSuffix :: String -> [String] -> [String]
addSuffix str txt = [x++str | x <- txt]

selectgt5 :: [Int] -> [Int]
selectgt5 selec = [x | x <- selec, x>5]

sumOdds :: [Int] -> Int
sumOdds soma = sum [x | x <- soma, odd x]

sumOdds' :: [Int] -> Int
sumOdds' somab = sum (map(\x -> if odd x then x else 0)somab)

selectExpr :: [Int] -> [Int]
selectExpr expr = [x | x <- expr, even x , x >= 20, x <= 50]

countShorts :: [String] -> Int
countShorts c = length [x | x <- c, length x < 5]

calcExpr :: [Float] -> [Float]
calcExpr calc = [x^2/2 | x <- calc , x^2/2 > 10]

trSpaces :: String -> String
trSpaces str1 = [if x == ' ' then '-' else x | x<- str1]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd lst2 = [y | (x,y) <- lst2]

dotProd :: [Int] -> [Int] -> Int
dotProd f g = sum [x*y | (x,y) <- zip f g]