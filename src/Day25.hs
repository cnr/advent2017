
module Main (main) where

import Control.Monad.State.Strict (State, execState, state)

-- hardcoded my input today

main :: IO ()
main = print (checksum (stepN 12919244 stateA))

stepN :: Int -> TState -> TZipper
stepN n tstate = execState (foldFixN n tstate) (TZipper [] False [])

foldFixN :: Monad m => Int -> Fix m -> m (Fix m)
foldFixN 0 fixed = return fixed
foldFixN n fixed = unFix fixed >>= foldFixN (n-1)

stateA :: TState
stateA = Fix . state $ \zipper ->
    if not (focus zipper)
       then (stateB, shiftR (setFocus True  zipper))
       else (stateC, shiftL (setFocus False zipper))

stateB :: TState
stateB = Fix . state $ \zipper ->
    if not (focus zipper)
       then (stateA, shiftL (setFocus True zipper))
       else (stateD, shiftR (setFocus True zipper))

stateC :: TState
stateC = Fix . state $ \zipper ->
    if not (focus zipper)
       then (stateA, shiftR (setFocus True  zipper))
       else (stateE, shiftL (setFocus False zipper))

stateD :: TState
stateD = Fix . state $ \zipper ->
    if not (focus zipper)
       then (stateA, shiftR (setFocus True  zipper))
       else (stateB, shiftR (setFocus False zipper))

stateE :: TState
stateE = Fix . state $ \zipper ->
    if not (focus zipper)
       then (stateF, shiftL (setFocus True zipper))
       else (stateC, shiftL (setFocus True zipper))

stateF :: TState
stateF = Fix . state $ \zipper ->
    if not (focus zipper)
       then (stateD, shiftR (setFocus True zipper))
       else (stateA, shiftR (setFocus True zipper))

type TState = Fix (State TZipper)

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
