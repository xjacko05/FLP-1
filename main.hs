import System.IO
import System.Environment
import Data.Typeable

data Point = Point { x :: Integer, y :: Integer } deriving (Read)
data Curve = Curve { p :: Integer
                   , a :: Integer
                   , b :: Integer
                   , g :: Point
                   , n :: Integer
                   , h :: Integer
                   } deriving (Read)

instance Show Point where
    show (Point x y) = "Point {\nx: " ++ show x ++ "\ny: " ++ show y ++ "\n}"

instance Show Curve where
    show (Curve p a b g n h) = "Curve {\np: " ++ show p ++ "\na: " ++ show a ++ "\nb: " ++ show b ++ "\ng: " ++ show g ++ "\nn: " ++ show n ++ "\nh: " ++ show h ++ "\n}\n"

main :: IO ()
main = do
    args <- getArgs
    let (mode, file) = parseArgs args
    input <- if (file=="") then getContents else readFile file
    let a = read input :: Curve
    let c = Curve {p= 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
                  ,a= 0
                  ,b= 7
                  ,g= Point {
                    x= 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
                   ,y= 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
                   }
                  ,n= 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
                  ,h= 1
                  }
    let k = map repl input
    let u = foldl insertComma [] k
    putStr (u)
    putStr (show c)
    putStr (show $ p a)
    return ()


parseArgs :: [String] -> (String, String)
parseArgs [x] = (x, "")
parseArgs [x,y] = (x, y)
parseArgs _ = error "Agument parsing failed."

repl :: Char -> Char
repl ':' = '='
repl c = c

insertComma :: [Char] -> Char -> String
insertComma acc c
    | c == 'a' = acc ++ ",a"
    | c == 'b' = acc ++ ",b"
    | c == 'g' = acc ++ ",g"
    | c == 'y' = acc ++ ",y"
    | c == 'n' = acc ++ ",n"
    | c == 'h' = acc ++ ",h"
    | otherwise = acc ++ [c]