module HW5sol where
import HW5types

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
rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP  p = rank p 0 

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (p:ps) r =  let (a,b) = rankC p in
		 if a > r then Nothing else rank ps ((r-a)+b)

-- semantic commands from HW 4
semCmd :: Cmd -> Stack -> Result
semCmd (LDI x) xs = A (I x : xs)
semCmd (LDB x) xs = A (B x : xs)

semCmd ADD (I x:I y:xs) = A (I (x + y) : xs)
semCmd ADD (_:_:_) = TypeError

semCmd MULT (I x:I y:xs) = A (I (x * y) : xs)
semCmd MULT (_:_:_) = TypeError

semCmd DUP (x:xs) = A (x : x : xs)

semCmd LEQ (I x:I y:xs) = A (B (x <= y) : xs)
semCmd LEQ (_:_:_) = TypeError

-- need to edit if else command
semCmd (IFELSE p1 p2) (B p:xs)
  | p = run p1 xs
  | otherwise = run p2 xs
semCmd (IFELSE _ _) (_:_) = TypeError

-- DEC decrements the topmost element on the stack
semCmd DEC (I x:xs) = A (I (x - 1) : xs)
semCmd DEC (_:_) = TypeError

-- SWAP exchanges the two topmost elements on the stack 
semCmd SWAP (x:y:xs) = A (y : x : xs)

-- POP k pops k elements off the stack
semCmd (POP k) xs
  | k > length xs = RankError
  | otherwise = A $ drop k xs

semCmd _ _ = RankError

-- run function 
run :: Prog -> Stack -> Result
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'
