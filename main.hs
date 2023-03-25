--ECDSA
--FLP 2023
--Autor: Martin Jacko <xjacko05>

import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randomR)
import Numeric (showHex)
import Data.Char (toUpper)

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
    show (Point x y) = "Point {\nx: " ++ toHexString x ++ "\ny: " ++ toHexString y ++ "\n}"

instance Show Key where
    show (Key d q) = "Key {\nd: " ++ ('0':'x':(showHex d "")) ++ "\nQ: " ++ toSEC q ++ "\n}"

instance Show Signature where
    show (Signature r s) = "Signature {\nr: " ++ ('0':'x':(showHex r "")) ++ "\ns: " ++ ('0':'x':(showHex s "")) ++ "\n}"

instance Show Curve where
    show (Curve p a b g n h) = "Curve {\np: " ++ toHexString p ++ "\na: " ++ show a ++ "\nb: " ++ show b ++ "\ng: " ++ show g ++ "\nn: " ++ toHexString n ++ "\nh: " ++ show h ++ "\n}"

toHexString :: Integer -> String
toHexString i = ('0':'x':[toUpper x | x <- (showHex i "")])

main :: IO ()
main = do
    args <- getArgs
    let (mode, file) = parseArgs args
    input <- if (file=="") then getContents else readFile file
    let curve = parseCurve input
    let key = parseKey input
    let signature = parseSignature input
    let hash = parseHash input
    gen <- getStdGen
    case mode of "-i" -> putStrLn (show curve)
                 "-k" -> putStrLn (show $ generateKey curve gen)
                 "-s" -> putStrLn (show $ sign curve key hash gen)
                 "-v" -> putStrLn (show $ verify curve signature key hash)
                 _ -> error "Invalid option used."

    --putStrLn (show signature)
    --putStrLn (show (fst (randomR (1, n curve) (gen) :: (Integer, StdGen))))
    --putStrLn (show $ addPoints (Point 13 16) (Point 0 0) 1 23)
    --putStrLn (show $ mulPoint (g curve) 91305095057638279798210088207290086814184648949849354342409048655369161716366 (a curve) (p curve))
    --putStrLn (show $ doublePoint (g curve) (a curve) (p curve))
    --putStrLn (show $ addPoints (mulPoint (g curve) 5 (a curve) (p curve)) (mulPoint (q key) 3 (a curve) (p curve)) (a curve) (p curve)) 
    return ()


getParam :: String -> String -> String
getParam param input = head $ tail (dropWhile (/=(param ++ ":")) (words input))

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

toSEC :: Point -> String
toSEC input = "0x04" ++ take (larger - length x') ['0','0'..] ++ x' ++ take (larger - length y') ['0','0'..] ++ y'
    where
        x' = showHex (x input) ""
        y' = showHex (y input) ""
        larger = if length x' > length y' then length x' else length y'



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

--getModInverse :: Integer -> Integer -> Integer
--getModInverse value modulo = head [x | x <- [0..(modulo - 1)], mod (value * x) modulo == 1]

addPoints' :: Point -> Point -> Integer -> Integer -> Point
addPoints' p q a modulo = Point coorX coorY
        where
    s
        | x p == x q && y p == y q = ((3*(x p)*(x p) + a) * (modInv (2*y p) modulo)) `mod` modulo
        | otherwise = ((y p - y q) * (modInv (x p - x q) modulo)) `mod` modulo
    coorX = mod (s*s - (x p) - (x q)) modulo
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
    | otherwise = mulPoint (doublePoint p a modulo) (div times 2) a modulo

doublePoint :: Point -> Integer -> Integer -> Point
doublePoint p a modulo = Point coorX coorY
        where
    s = mod ((3*(x p)*(x p) + a) * (modInv (2*(y p)) modulo)) modulo
    coorX = mod (s*s - 2*(x p)) modulo
    coorY = mod (s * (x p - coorX) - y p) modulo


generateKey :: Curve -> StdGen -> Key
generateKey curve gen = Key d q
    where
        d = fst (randomR (1, (n curve) - 1) (gen) :: (Integer, StdGen))
        q = mulPoint (g curve) d (a curve) (p curve)

sign :: Curve -> Key -> Hash -> StdGen -> Signature
sign curve key hash gen = Signature r s
    where
        (r,k) = getR curve gen
        s = mod ((modInv k (n curve)) * (hash + r*(d key))) (n curve)

getR :: Curve -> StdGen -> (Integer, Integer)
getR curve gen = (r,k)
    where
        (kTmp, gen1) = (randomR (1, (n curve) - 1) (gen) :: (Integer, StdGen))
        kG = mulPoint (g curve) kTmp (a curve) (p curve)
        rTmp = mod (x kG) (n curve)
        (r,k)
            | rTmp == 0 = getR curve gen1
            | otherwise = (rTmp,kTmp)



verify :: Curve -> Signature -> Key -> Hash -> Bool
verify curve signature key hash = val
    where
        w = mod (modInv (s signature) (n curve)) (n curve)
        u1 = mod (w*hash) (n curve)
        u2 = mod ((r signature)*w) (n curve)
        xy = addPoints (mulPoint (g curve) u1 (a curve) (p curve)) (mulPoint (q key) u2 (a curve) (p curve)) (a curve) (p curve)
        val = (if mod (r signature) (n curve) == mod (x xy) (n curve) then True else False)






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