module HW5sol where
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
rankC (LD i) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC DEC = (1,1)
rankC SWAP = (2,2)
rankC (POP i) = (i,0)

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

semCmd (LDI i) s = Just (Right i:s)
semCmd (LDB b) s = Just (Left b:s)

semCmd ADD [] = Nothing
semCmd ADD ((Right x):[]) = Nothing
semCmd ADD ((Right x):(Right y):s) = Just ((Right(x+y)):s)

semCmd MULT [] = Nothing
semCmd MULT ((Right x):[]) = Nothing
semCmd MULT ((Right x):(Right y):s) = Just ((Right(x*y)):s)

semCmd DUP [] = Nothing
semCmd DUP (x:xs) = Just (x:x:xs)

semCmd LEQ [] = Nothing
semCmd LEQ ((Right x):[]) = Nothing
semCmd LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s)
semCmd LEQ _ = Nothing

semCmd (IFELSE p1 p2) (Left True:xs) = run p1 xs
semCmd (IFELSE p1 p2) (Left False:xs) = run p2 xs

run :: Prog -> Stack -> Result
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
