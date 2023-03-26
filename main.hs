--ECDSA
--FLP FUN 2023
--Autor: Martin Jacko (222092) <xjacko05@stud.fit.vutbr.cz>

import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randomR)
import Numeric (showHex)
import Data.Char (toUpper)

--custom data types
data Point = Point { x :: Integer, y :: Integer }
data Curve = Curve { p :: Integer,
                     a :: Integer,
                     b :: Integer,
                     g :: Point,
                     n :: Integer,
                     h :: Integer
                   }
data Key = Key { d :: Integer, q :: Point }
data Signature = Signature { r :: Integer, s :: Integer }
type Hash = Integer

--show instances according to specification
instance Show Point where
    show (Point x y) = "Point {\nx: " ++ toHexString x ++ "\ny: " ++ toHexString y ++ "\n}"

instance Show Key where
    show (Key d q) = "Key {\nd: " ++ ('0':'x':(showHex d "")) ++ "\nQ: " ++ toSEC q ++ "\n}"

instance Show Signature where
    show (Signature r s) = "Signature {\nr: " ++ ('0':'x':(showHex r "")) ++ "\ns: " ++ ('0':'x':(showHex s "")) ++ "\n}"

instance Show Curve where
    show (Curve p a b g n h) = "Curve {\np: " ++ toHexString p ++ "\na: " ++ show a ++ "\nb: " ++ show b ++ "\ng: " ++ show g ++ "\nn: " ++ toHexString n ++ "\nh: " ++ show h ++ "\n}"

--converts Integer from decimal to hex format
toHexString :: Integer -> String
toHexString i = ('0':'x':[toUpper x | x <- (showHex i "")])

--converts point from x & y coordinates to SEC uncompressed format
toSEC :: Point -> String
toSEC input = "0x04" ++ take (larger - length x') ['0','0'..] ++ x' ++ take (larger - length y') ['0','0'..] ++ y'
    where
        x' = showHex (x input) ""
        y' = showHex (y input) ""
        larger = if length x' > length y' then length x' else length y'

--reads inputs and acts accordingly
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
    return ()


--parses arguemnts
parseArgs :: [String] -> (String, String)
parseArgs [x] = (x, "")
parseArgs [x,y] = (x, y)
parseArgs _ = error "Incorrect options used"

--searches input for parameter value in string form
getParam :: String -> String -> String
getParam param input = value
    where
        parsed = dropWhile (/=(param ++ ":")) (words input)
        --checks if target value present in input
        value
            | parsed == [] = error ("Value of " ++ param ++ " not found.")
            | otherwise = head $ tail parsed

--parses curve from input
parseCurve :: String -> Curve
parseCurve input = checkedCurve
    where
        curve = Curve { p= read(getParam "p" input) :: Integer,
                        a= read(getParam "a" input) :: Integer,
                        b= read(getParam "b" input) :: Integer,
                        g= Point {
                            x= read(getParam "x" input) :: Integer,
                            y= read(getParam "y" input) :: Integer
                        },
                        n= read(getParam "n" input) :: Integer,
                        h= read(getParam "h" input) :: Integer
                        }
        --checks if a,b,g parameters compose an ec
        checkedCurve
            | checkCurveSingularity curve && checkCurvePoint curve (g curve) = curve
            | otherwise = error ("Curve values not correct")

--parses curve from input
parseKey :: String -> Key
parseKey input = Key { d= read(getParam "d" input) :: Integer, q= fromSEC $ getParam "Q" input }

--converts point from SEC uncompressed format to x & y coordinates
fromSEC :: String -> Point
fromSEC input = Point x y
    where
        base = drop 4 input
        len = div (length base) 2
        x = read ("0x" ++ take len base) :: Integer
        y = read ("0x" ++ drop len base) :: Integer

--parses siganture from input
parseSignature :: String -> Signature
parseSignature input = Signature { r= read(getParam "r" input) :: Integer, s= read(getParam "s" input) :: Integer }

--parses hash from input
parseHash :: String -> Hash
parseHash input = read(getParam "Hash" input) :: Integer

--checks if given curve satisfies non-singularity test (4a^3 + 27b^2 != 0)
checkCurveSingularity :: Curve -> Bool
checkCurveSingularity curve
    | 4*(a curve)*(a curve)*(a curve) + 27*(b curve)*(b curve) == 0 = False
    | otherwise = True

--check if given point is on the curve, by checking whether it satisfies eliptic curve equation (y^2 = x^3 +ax + b) mod p
checkCurvePoint :: Curve -> Point -> Bool
checkCurvePoint c k
    | (mod ((y k)*(y k)) (p c)) == (mod ((x k)*(x k)*(x k) + (a c)*(x k) + (b c)) (p c)) = True
    | otherwise = False

--calculates modular inverse using EEA
modInverse :: Integer -> Integer -> Integer
modInverse a modulo = (if x < 0 then x + modulo else x)
  where
    (_, x, _) = euclid a modulo

--extendend Euclidean Algorithm - recursively looks for gcd, then calculates p and q
euclid :: Integer -> Integer -> (Integer, Integer, Integer)
euclid a 0 = (a, 1, 0)
euclid a modulo = (gcd', y, x - q * y)
    where
        q = div a modulo
        r = mod a modulo
        (gcd', x, y) = euclid modulo r

--auxiliary function for adding points in integer ec arithmetic
addDifferentPoints :: Point -> Point -> Integer -> Point
addDifferentPoints p q modulo = Point coorX coorY
    where
        s = ((y p - y q) * (modInverse (x p - x q) modulo)) `mod` modulo
        coorX = mod (s*s - (x p) - (x q)) modulo
        coorY = mod (s * (x p - coorX) - y p) modulo

--adds points in integer ec arithmetic
addPoints :: Point -> Point -> Integer -> Integer -> Point
addPoints p q a modulo
    | x p == y p && y p == 0 = q
    | x q == y q && y q == 0 = p
    | x p == x q && y p /= y q = Point 0 0
    | x p == x q && y p == y q && y p == 0 = Point 0 0
    | x p == x q && y p == y q = doublePoint p a modulo
    | otherwise = addDifferentPoints p q modulo

--doubles point in integer ec arithmetic
doublePoint :: Point -> Integer -> Integer -> Point
doublePoint p a modulo = Point coorX coorY
    where
        s = mod ((3*(x p)*(x p) + a) * (modInverse (2*(y p)) modulo)) modulo
        coorX = mod (s*s - 2*(x p)) modulo
        coorY = mod (s * (x p - coorX) - y p) modulo

--multiplies point in integer ec arithmetic
mulPoint :: Point -> Integer -> Integer -> Integer -> Point
mulPoint p times a modulo
    | times == 0 = Point 0 0
    | mod times 2 == 1 = addPoints (mulPoint (doublePoint p a modulo) (div times 2) a modulo) p a modulo
    | otherwise = mulPoint (doublePoint p a modulo) (div times 2) a modulo

--generates new keypair
generateKey :: Curve -> StdGen -> Key
generateKey curve gen = Key d q
    where
        d = fst (randomR (1, (n curve) - 1) (gen) :: (Integer, StdGen))
        q = mulPoint (g curve) d (a curve) (p curve)

--calucates "r" and random number "k", which are used during signing process
getR :: Curve -> StdGen -> (Integer, Integer)
getR curve gen = (r,k)
    where
        (kTmp, gen1) = (randomR (1, (n curve) - 1) (gen) :: (Integer, StdGen))
        kG = mulPoint (g curve) kTmp (a curve) (p curve)
        rTmp = mod (x kG) (n curve)
        --checks if "k" is of fitting value
        (r,k)
            | rTmp == 0 = getR curve gen1
            | otherwise = (rTmp,kTmp)

--signs hash using given key
sign :: Curve -> Key -> Hash -> StdGen -> Signature
sign curve key hash gen = checkedSignature
    where
        (r,k) = getR curve gen
        s = mod ((modInverse k (n curve)) * (hash + r*(d key))) (n curve)
        --checks if point Q is on the curve
        checkedSignature
            | not (checkCurvePoint curve (q key)) = error "Q is not on curve"
            | otherwise = Signature r s

--verifies given signature
verify :: Curve -> Signature -> Key -> Hash -> Bool
verify curve signature key hash = val
    where
        w = mod (modInverse (s signature) (n curve)) (n curve)
        u1 = mod (w*hash) (n curve)
        u2 = mod ((r signature)*w) (n curve)
        xy = addPoints (mulPoint (g curve) u1 (a curve) (p curve)) (mulPoint (q key) u2 (a curve) (p curve)) (a curve) (p curve)
        --checks if point Q is on the curve
        val
            | not (checkCurvePoint curve (q key)) = error "Q is not on curve"
            | otherwise = (if mod (r signature) (n curve) == mod (x xy) (n curve) then True else False)