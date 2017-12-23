
module Main (main) where

import Common
import Data.Function
import Data.List
import Data.Ord
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = do
    particles <- readParsedLines particleP 20

    print (part1 (zip [0..] particles))
    print (part2 particles)

part1 :: [(Int,Particle)] -> (Int,Particle)
part1 = head . sortBy (comparing (dist . posnAt 1000 . snd))

part2 :: [Particle] -> Int
part2 = length . (!! 1000) . iterate stepCollide

type Vector = (Int,Int,Int)

data Particle = Particle { posn         :: Vector
                         , velocity     :: Vector
                         , acceleration :: Vector
                         } deriving Show

stepCollide :: [Particle] -> [Particle]
stepCollide = concat . filter ((== 1) . length) . groupBy ((==) `on` posn) . sortBy (comparing posn) . map posnStep

posnStep :: Particle -> Particle
posnStep (Particle (px,py,pz) (vx,vy,vz) (ax,ay,az)) =
    let vx' = vx + ax
        vy' = vy + ay
        vz' = vz + az
     in (Particle (px+vx',py+vy',pz+vz') (vx',vy',vz') (ax,ay,az))

posnAt :: Int -> Particle -> Vector
posnAt n (Particle (px,py,pz) (vx,vy,vz) (ax,ay,az)) = (combine px vx ax, combine py vy ay, combine pz vz az)
    where
    combine p v a = p + n * v + (n * (n+1) `div` 2) * a

dist :: Vector -> Int
dist (x,y,z) = abs x + abs y + abs z

particleP :: Parser Particle
particleP = Particle <$ string "p=" <*> vectorP <* string ", v=" <*> vectorP <* string ", a=" <*> vectorP

vectorP :: Parser Vector
vectorP = between (char '<') (char '>') $ do
    [x,y,z] <- intP `sepBy1` char ','
    return (x,y,z)

intP :: Parser Int
intP = negate <$ char '-' <*> (read <$> some digitChar)
   <|>                         read <$> some digitChar
