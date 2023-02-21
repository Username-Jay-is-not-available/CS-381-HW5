\module HW5sol where
import HW5types

type Prog = [Cmd]
data Cmd 
        = LDI Int
        | LDB Bool
        | LEQ
        | ADD
        | MULT
        | DUP
        | IFELSE Prog Prog
        | DEC
        | SWAP
        | POP Int
        deriving Show

data Val = I Int | B Bool
data Stack = [Val]

type Rank = Int 
type CmdRank = (Int,Int)

-- rank C that maps each stack operation to its rank
rankC :: Cmd -> CmdRank
rankC (LDI i) = (0,1)
rankC (LDB b) = (0,1)
rankC LEQ = (2,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC IFELSE = (1,0)
rankC DEC = (1,1)
rankC SWAP = (2,2)
rankC (POP k) = (k,0)

-- rank P that computes the rank of a program when ran with a stack of rank r
rankP :: Prog -> Rank -> Maybe Rank
rankP [] = Just 0
rankP  p = rank p 0 

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (p:ps) r =  let (a,b) = rankC p in
		 if a > r then Nothing else rank ps ((r-a)+b)

-- semantic commands from HW 4
semCmd :: Cmd ->Stack -> Maybe Stack

semCmd (LDI i) s = Just (Left i:s)
semCmd (LDB b) s = Just (Right b:s)

semCmd ADD [] = Nothing
semCmd ADD ((Left x):[]) = Nothing
semCmd ADD ((Left x):(Left y):s) = Just ((Left(x+y)):s)

semCmd MULT [] = Nothing
semCmd MULT ((Left x):[]) = Nothing
semCmd MULT ((Left x):(Left y):s) = Just ((Left(x*y)):s)

semCmd DUP [] = Nothing
semCmd DUP (x:xs) = Just (x:x:xs)

semCmd LEQ [] = Nothing
semCmd LEQ ((Left x):[]) = Nothing
semCmd LEQ ((Left x):(Left y):s) = Just ((Left(x<=y)):s)
semCmd LEQ _ = Nothing

-- need to edit if else command
semCmd (IFELSE p1 p2) (Right True:xs) = run p1 xs
semCmd (IFELSE p1 p2) (Right False:xs) = run p2 xs

-- DEC decrements the topmost element on the stack
semCmd DEC [] = Nothing
semCmd DEC (x:xs) = ((x-1):xs)

-- SWAP exchanges the two topmost elements on the stack 
semCmd SWAP [] = Nothing
semCmd SWAP (x:y:xs) = (y:x:xs)

-- POP k pops k elements off the stack
semCmd (POP k) s = (drop k) s

-- run function 
run :: Prog -> Stack -> Result
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
