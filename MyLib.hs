module MyLib where

import Data.Matrix (multStd, Matrix, elementwise, colVector, transpose, diagonal, fromLists, mapPos, toList)
-- import qualified Data.List.NonEmpty as NE (NonEmpty, zipWith, map)
import Data.Vector (fromList)
import Data.Bifunctor (bimap)
import Control.Monad (join)

type Vec = [Double]
type NRInput = Vec
type NROutput = Vec
type NRSample = (NRInput, NROutput)
type NRBatch = [NRSample]
type NRMiniBatch = [NRSample]
type NRPass = (Matrix Double, [Double])
weights = fst
biases = snd
type NRNetwork = [NRPass]
type DifferentialNetwork = NRNetwork

(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

linearT :: Num f => Matrix f -> [f] -> [f]
linearT m l = toList $ multStd m (colVector (fromList l))

mAdd :: Num f => Matrix f -> Matrix f -> Matrix f
mAdd = elementwise (+)

vAdd :: Num f => [f] -> [f] -> [f]
vAdd = zipWith (+)

pAdd :: NRPass -> NRPass -> NRPass
pAdd (x,y) (a,b) = (mAdd x a, vAdd y b)

affineT :: NRPass -> [Double] -> [Double]
affineT pass prevA = (linearT (weights pass) prevA) `vAdd` (biases pass)

sigmoid :: Double -> Double
sigmoid x = 1/(1+ exp (-x))

activate = (map sigmoid) .* affineT

-- |Derivative of sigmoid.
sigmoid' :: Double -> Double
sigmoid' x = let s = sigmoid x
             in s * (1 - s)

feedforward :: NRInput-> NRNetwork -> NROutput 
feedforward = foldr activate

expectation :: (Foldable f) => f Double -> Double
expectation samples = (sum samples) / (fromIntegral $ length samples)

gradF :: NRNetwork -> NRInput -> NROutput -> [Double]
gradF [] a y = map (2*) (zipWith (-) a y)
gradF net a y = let (wb:net_) = net
                    t = affineT wb a
                    sigvec' = fromList $ map sigmoid' t
                    sigMat' = diagonal 0 sigvec'
                    jt = transpose $ sigMat' `multStd` (weights wb)
                    a_ = activate wb a
                    gF_ = gradF net_ a_ y
                in jt `linearT` gF_


-- |'a' is output dimension
gradOmega :: Int -> NRInput -> Matrix Double
gradOmega = fromLists .* replicate 

gradW :: NRNetwork -> NRInput -> NROutput -> Matrix Double
gradW net a y = let (wb:net_) = net
                    a_ = activate wb a
                    gO = gradOmega (length a_) a
                    gF_ = gradF net_ a_ y
                 in mapPos (\(i,j) x -> x * (gF_ !! i) ) gO

gradCx :: NRNetwork -> NRSample -> NRNetwork
gradCx [] _ = []
gradCx net (a,y) = let (wb:net_) = net
                       a_ = activate wb a
                       gW = gradW net a y
                       gB = gradF net_ a_ y
                       in  ((gW, gB):gradCx net_ (a_, y) )

eatToTail :: [a] -> [[a]]
eatToTail [] = []
eatToTail xs = xs : eatToTail (tail xs)

gradBatch :: NRNetwork -> [NRSample] -> NRNetwork
gradBatch net samples =
    let gCxs = map (gradCx net) samples
    in foldr1 (zipWith pAdd) gCxs

flatPass :: NRPass -> [Double]
flatPass (w,b) = toList w ++ b

flatNetwork :: DifferentialNetwork -> [Double]
flatNetwork = join . map flatPass

mapPass :: (Double -> Double) -> NRPass -> NRPass
mapPass f = bimap (mapPos (\_ -> f)) (map f)

mapNetwork :: (Double -> Double) -> NRNetwork -> NRNetwork
mapNetwork f = map (mapPass f)

magnitude = sqrt . sum . map (^2)

zipWithPass :: (Double -> Double -> Double) -> NRPass -> NRPass -> NRPass
zipWithPass f (x,y) (a,b) = (elementwise f x a, zipWith f y b)

zipWithNetwork :: (Double -> Double -> Double) -> NRNetwork -> NRNetwork -> NRNetwork
zipWithNetwork f = zipWith (zipWithPass f)

normalize :: DifferentialNetwork -> DifferentialNetwork
normalize net = 
    let mag = magnitude (flatNetwork net)
    in mapNetwork (/mag) net

train :: [NRBatch] -> NRNetwork -> NRNetwork
train [] net = net
train (b:restB) net =
    let gC = gradBatch net b
    in zipWithNetwork (+) net (mapNetwork (*0.0001) (normalize gC))