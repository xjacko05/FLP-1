import System.IO
import System.Environment
--import System.Random

data Point = Point { x :: Integer, y :: Integer } deriving (Read, Eq)
data Curve = Curve { p :: Integer
                   , a :: Integer
                   , b :: Integer
                   , g :: Point
                   , n :: Integer
                   , h :: Integer
                   }
data Key = Key { d :: Integer, q :: Integer }
data Signature = Signature { r :: Integer, s :: Integer }
type Hash = Integer

instance Show Point where
    show (Point x y) = "Point {\nx: " ++ show x ++ "\ny: " ++ show y ++ "\n}"

instance Show Key where
    show (Key d q) = "Key {\nd: " ++ show d ++ "\nQ: " ++ show q ++ "\n}"

instance Show Signature where
    show (Signature r s) = "Signature {\nr: " ++ show r ++ "\ns: " ++ show s ++ "\n}"

instance Show Curve where
    show (Curve p a b g n h) = "Curve {\np: " ++ show p ++ "\na: " ++ show a ++ "\nb: " ++ show b ++ "\ng: " ++ show g ++ "\nn: " ++ show n ++ "\nh: " ++ show h ++ "\n}\n"

main :: IO ()
main = do
    args <- getArgs
    let (mode, file) = parseArgs args
    input <- if (file=="") then getContents else readFile file
    let curve = parseCurve input
    let key = parseKey input
    let signature = parseSignature input
    let hash = parseHash input
    --putStr (u)
    --putStr (show c)
    --putStrLn (show (getParam "y" (words input)))
    case mode of "-i" -> putStrLn (show curve)

    --putStrLn (show signature)
    putStrLn (show $ addPoints (Point 13 16) (Point 0 0) 1 23)
    putStrLn (show $ mulPoint (Point 13 16) 158158158 1 23)
    return ()


getParam :: String -> [String] -> Integer
getParam param arr =  read (head $ tail (dropWhile (/=(param ++ ":")) arr)) :: Integer

parseCurve :: String -> Curve
parseCurve input = Curve { p= getParam "p" (words input),
                            a= getParam "a" (words input),
                            b= getParam "b" (words input),
                            g= Point {
                                x= getParam "x" (words input),
                                y= getParam "y" (words input)
                            },
                            n= getParam "n" (words input),
                            h= getParam "h" (words input)
                        }

parseKey :: String -> Key
parseKey input = Key {  d= getParam "d" (words input),
                        q= getParam "Q" (words input)
                    }

parseSignature :: String -> Signature
parseSignature input = Signature {  r= getParam "r" (words input),
                                    s= getParam "s" (words input)
                                }
parseHash :: String -> Hash
parseHash input = getParam "Hash" (words input)


parseArgs :: [String] -> (String, String)
parseArgs [x] = (x, "")
parseArgs [x,y] = (x, y)
parseArgs _ = error "Agument parsing failed."

getModInverse :: Integer -> Integer -> Integer
getModInverse value modulo = head [x | x <- [0..(modulo - 1)], mod (value * x) modulo == 1]

addPoints' :: Point -> Point -> Integer -> Integer -> Point
addPoints' p q a modulo = Point coorX coorY
        where
    s
        | x p == x q && y p == y q = ((3*(x p)^2 + a) * (getModInverse (2*y p) modulo)) `mod` modulo
        | otherwise = ((y p - y q) * (getModInverse (x p - x q) modulo)) `mod` modulo
    coorX = mod (s^2 - (x p) - (x q)) modulo
    coorY = mod (s * (x p - coorX) - y p) modulo

addPoints :: Point -> Point -> Integer -> Integer -> Point
addPoints p q a modulo
    | x p == y p && y p == 0 = q
    | x q == y q && y q == 0 = p
    | x p == x q && y p /= y q = Point 0 0
    | x p == x q && y p == y q && y p == 0 = Point 0 0
    | otherwise = addPoints' p q a modulo

mulPoint :: Point -> Integer -> Integer -> Integer -> Point
mulPoint p times a modulo
    | times == 1 = p
    | otherwise = addPoints (mulPoint p (times-1) a modulo) p a modulo