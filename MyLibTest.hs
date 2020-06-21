module Main where
import Data.Matrix
import MyLib

m22 :: Matrix Double
m22 = matrix 2 2 $ \(i, j) -> fromIntegral (2*i - j)

v2 = [-3.0, -4.0]

main :: IO ()
main = do
    print $ affineT (m22, v2) [1.0,2.0]
