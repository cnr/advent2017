
module Main (main) where

-- hardcoded my input today

main :: IO ()
main = print (checksum (stepN 12919244 stateA))

-- I feel like there's a better way to tie this together
stepN :: Int -> Fix TState -> TZipper
stepN = go (TZipper [] False [])
    where
    go :: TZipper -> Int -> Fix TState -> TZipper
    go zipper 0 _     = zipper
    go zipper n state = let (state', zipper') = runTState (unFix state) zipper
                           in go zipper' (n-1) state'

stateA :: Fix TState
stateA = Fix . TState $ \zipper ->
    if not (focus zipper)
       then (stateB, shiftR (setFocus True  zipper))
       else (stateC, shiftL (setFocus False zipper))

stateB :: Fix TState
stateB = Fix . TState $ \zipper ->
    if not (focus zipper)
       then (stateA, shiftL (setFocus True zipper))
       else (stateD, shiftR (setFocus True zipper))

stateC :: Fix TState
stateC = Fix . TState $ \zipper ->
    if not (focus zipper)
       then (stateA, shiftR (setFocus True  zipper))
       else (stateE, shiftL (setFocus False zipper))

stateD :: Fix TState
stateD = Fix . TState $ \zipper ->
    if not (focus zipper)
       then (stateA, shiftR (setFocus True  zipper))
       else (stateB, shiftR (setFocus False zipper))

stateE :: Fix TState
stateE = Fix . TState $ \zipper ->
    if not (focus zipper)
       then (stateF, shiftL (setFocus True zipper))
       else (stateC, shiftL (setFocus True zipper))

stateF :: Fix TState
stateF = Fix . TState $ \zipper ->
    if not (focus zipper)
       then (stateD, shiftR (setFocus True zipper))
       else (stateA, shiftR (setFocus True zipper))

newtype TState a = TState { runTState :: TZipper -> (a, TZipper) }

data Fix f = Fix { unFix :: f (Fix f) }

data TZipper = TZipper [Bool] Bool [Bool]

checksum :: TZipper -> Int
checksum (TZipper ls x rs) = length [() | True <- (x : ls ++ rs)]

setFocus :: Bool -> TZipper -> TZipper
setFocus y (TZipper ls _ rs) = TZipper ls y rs

focus :: TZipper -> Bool
focus (TZipper _ x _) = x

shiftL :: TZipper -> TZipper
shiftL (TZipper []     x rs) = TZipper [] False (x:rs)
shiftL (TZipper (l:ls) x rs) = TZipper ls l     (x:rs)

shiftR :: TZipper -> TZipper
shiftR (TZipper ls x []    ) = TZipper (x:ls) False []
shiftR (TZipper ls x (r:rs)) = TZipper (x:ls) r     rs
