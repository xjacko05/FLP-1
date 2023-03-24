import System.IO
import System.Environment
import System.Random

data Point = Point { x :: Integer, y :: Integer } deriving (Read, Eq)
data Curve = Curve { p :: Integer
                   , a :: Integer
                   , b :: Integer
                   , g :: Point
                   , n :: Integer
                   , h :: Integer
                   }
data Key = Key { d :: Integer, q :: Point }
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
    gen <- getStdGen
    case mode of "-i" -> putStrLn (show curve)
                 "-k" -> putStrLn (show $ generateKey curve gen)

    --putStrLn (show signature)
    --putStrLn (show (fst (randomR (1, n curve) (gen) :: (Integer, StdGen))))
    --putStrLn (show $ addPoints (Point 13 16) (Point 0 0) 1 23)
    putStrLn (show $ mulPoint (g curve) 91305095057638279798210088207290086814184648949849354342409048655369161716366 (a curve) (p curve))
    --putStrLn (show $ doublePoint (g curve) (a curve) (p curve))
    return ()


getParam :: String -> String -> String
getParam param input =  head $ tail (dropWhile (/=(param ++ ":")) (words input))

parseCurve :: String -> Curve
parseCurve input = Curve { p= read(getParam "p" input) :: Integer,
                            a= read(getParam "a" input) :: Integer,
                            b= read(getParam "b" input) :: Integer,
                            g= Point {
                                x= read(getParam "x" input) :: Integer,
                                y= read(getParam "y" input) :: Integer
                            },
                            n= read(getParam "n" input) :: Integer,
                            h= read(getParam "h" input) :: Integer
                        }

parseKey :: String -> Key
parseKey input = Key {  d= read(getParam "d" input) :: Integer,
                        q= fromSEC $ getParam "Q" input
                    }

fromSEC :: String -> Point
fromSEC input = Point x y
    where
        base = drop 4 input
        len = div (length base) 2
        x = read ("0x" ++ take len base) :: Integer
        y = read ("0x" ++ drop len base) :: Integer



parseSignature :: String -> Signature
parseSignature input = Signature {  r= read(getParam "r" input) :: Integer,
                                    s= read(getParam "s" input) :: Integer
                                }
parseHash :: String -> Hash
parseHash input = read(getParam "Hash" input) :: Integer


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
        | x p == x q && y p == y q = ((3*(x p)^2 + a) * (modInv (2*y p) modulo)) `mod` modulo
        | otherwise = ((y p - y q) * (modInv (x p - x q) modulo)) `mod` modulo
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
    | times == 0 = Point 0 0
    | mod times 2 == 1 = addPoints (mulPoint (doublePoint p a modulo) (div times 2) a modulo) p a modulo
    | mod times 2 == 0 = mulPoint (doublePoint p a modulo) (div times 2) a modulo

doublePoint :: Point -> Integer -> Integer -> Point
doublePoint p a modulo = Point coorX coorY
        where
    s = mod ((3*(x p)^2 + a) * (modInv (2*(y p)) modulo)) modulo
    coorX = mod (s^2 - 2*(x p)) modulo
    coorY = mod (s * (x p - coorX) - y p) modulo


generateKey :: Curve -> StdGen -> Key
generateKey curve gen = Key d q
    where
        d = fst (randomR (1, n curve) (gen) :: (Integer, StdGen))
        q = mulPoint (g curve) d (a curve) (p curve)








--TODO
modInv :: Integer -> Integer -> Integer
modInv a p = (if x < 0 then x + p else x)
  where
    (_, x, _) = eea a p

eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea a 0 = (a, 1, 0)
eea a b = (g, y, x - q * y)
    where
        q = div a b
        r = mod a b
        (g, x, y) = eea b r