module Assembler where
import Data.List
import System.IO

type State = (Int, [Int])

data Instr = Add {- (a b -- a + b) -}
           | And {- (a b -- a /\ b) -}
           | Or {- (a b -- a \/ b) -}
           | Not {- (a -- 1 if a=0 else 0) -}
           | Lt {- (a b -- a < b) -}
           | Eq {- (a b -- a = b) -}
           | Pop {- (a -- ) -}
           | Push Int {- ( -- a) a is the Int -}
           | Swap {- (a b -- b a) -}
           | Dup {- (a -- a a) -}
           | Over {- (a b c -- a b c a) -}
           | RotL {- (a b c -- b c a) -}
           | Read {- ( -- a ) a is read from console in base 10 -}
           | Print {- (a -- ) a is printed to console in base 10 -}
           | JmpIf {- (cond addr -- ) pc := addr if cond =/= 0 -}
           | Store {- (addr val -- ) mem[addr] := val -}
           | Load {- (addr -- mem[addr]) -}
           | Done deriving (Eq, Show)

int2Bool :: Int -> Bool
int2Bool 0 = False
int2Bool _ = True

bool2Int :: Bool -> Int
bool2Int False = 0
bool2Int True = 1 

step :: State -> Instr -> State
step (pc, l:r:stack) Add = (pc+1, l+r : stack)
step (pc, l:r:stack) And = (pc+1, bool2Int ((int2Bool l) && (int2Bool r)) : stack)
step (pc, l:r:stack) Eq = (pc+1, bool2Int (l==r) : stack)
step (pc, stack) (Push i) = (pc+1, i:stack)
step (pc, l:r:stack) Swap = (pc+1, r:l:stack)

run :: [Instr] -> State -> State
run [] s = s
run (i:is) s = run is (step s i)
