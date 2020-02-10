
-- Prim op cases
-- 10 + --> operator and a single integer, has to look for another integer
-- 1 0 + --> operator and two integers, should evaluate the result immedilate
-- + + + --> operator followed by other operators, needs to recursively evaluate 

-- Word parsing
-- see a : and must stick the next word into a Word data type. 
-- Keep taking instructions until we run into a ;
-- Need to throw the Word instructions into a Dict that can be used later to look up instructions
-- Dict probably can have an expr list as it's second argument instead of a string. 
-- Write a data type for the pre-defined functions that has the list of expressions readily available (dup, etc)
-- Write a data type for if then else



{-- 

TODO: 
    - Clean up the take 2 and drop 2 if possible
--}

type Stack = [Term]
type Dict = [(String, [Term])]

data Term = 
    Const Int 
    | Word String [Term] 
    | PrimOp Op 
    | If Term Term Term

data AbstractMachine = AbstractMachine {
      stack :: Stack
    , dictionary :: Dict
    , terms :: [Term]
}

data Op = 
      Add (Int -> Int -> Int) 
    | Sub (Int -> Int -> Int)
    | Mult (Int -> Int -> Int)
    | Div (Int -> Int -> Int)

instance Show Term where
    show ex = case ex of
        Const e -> " Const " ++ show e
        Word s e -> " Word " ++ s ++ (concatMap show e)
        PrimOp e -> " PrimOp " ++ " <function> "  


-- emptyMachine :: AbstractMachine
-- emptyMachine = AbstractMachine { stack = [], dictionary = [] }

push :: Term -> Stack -> Stack
push e stack = e:stack 

-- Will throw error on empty list
pop :: Stack -> (Term, Stack)
pop (e:stack) = (e, stack) 

peek :: Stack -> Term
peek (e:stack) = e

evalPrimOp :: Op -> [Term] -> Term
evalPrimOp _ [] = error("Cannot eval primop on empty list")
evalPrimOp _ ([x]) = error("Cannot eval primop with single argument")
evalPrimOp op (x:y:xs) = case op of 
    Add f -> Const (f (primVal x) (primVal y))
    Sub f -> Const (f (primVal y) (primVal x))
    Mult f -> Const (f (primVal x) (primVal y))
    Div f -> Const (f (primVal y) (primVal x))


primVal :: Term -> Int
primVal (Const a) = a

toTerm :: [String]  -> [Term]
toTerm [] = []
toTerm (x:xs)
    | x == "+" = (PrimOp $ Add (+)) : toTerm xs
    | x == "-" = (PrimOp $ Sub (-)) : toTerm xs
    | x == "*" = (PrimOp $ Mult (*)) : toTerm xs
    | x == "/" = (PrimOp $ Div (div)) : toTerm xs
    | x == ":" = wordToTerm xs : toTerm (dropWord xs)
    | x == ";" = toTerm xs
    | otherwise = (Const (read x :: Int)) : toTerm xs


genMachine :: [String] -> Dict -> ([Term], Dict)
genMachine [] d = ([], d)
genMachine (x:xs) d = case x of 
    "+" -> ((PrimOp $ Add (+)) : terms, dict)
            where (terms, dict) = genMachine xs d
    "-" -> ((PrimOp $ Sub (-)) : terms, dict)
            where (terms, dict) = genMachine xs d
    "*" -> ((PrimOp $ Mult (*)) : terms, dict)
            where (terms, dict) = genMachine xs d
    "/" -> ((PrimOp $ Div (div)) : terms, dict)
            where (terms, dict) = genMachine xs d
    ":" -> (word : terms,  dict)
            where word@(Word name ts) = wordToTerm xs 
                  updatedDict = (name, ts):d
                  (terms, dict) = genMachine (dropWord xs) updatedDict
    ";" -> genMachine xs d
    val  -> case (lookup val d) of
                Just ts -> (ts ++ terms, dict)
                    where (terms, dict) = genMachine xs d
                Nothing -> ((Const (read x :: Int)) : terms, dict)
                     where (terms, dict) = genMachine xs d
                
            
        

wordToTerm :: [String] -> Term
wordToTerm xs = setWord (parseWord xs)

parseWord :: [String] -> [String]
parseWord xs = takeWhile (/= ";") xs

dropWord :: [String] -> [String]
dropWord xs = dropWhile (/= ";") xs


setWord :: [String] -> Term
setWord [] = error("Could not define word for empty list")
setWord [x] = error("Cannot define word without instructions")
setWord (x:xs) = Word x (toTerm xs)


eval :: Stack -> Stack -> Stack
eval [] s = s
eval (x:xs) s = case x of 
        Const a -> eval xs (push (Const a) s)
        PrimOp f ->  eval xs (push result (drop 2 s))
            where result = evalPrimOp f (take 2 s)
        Word name terms -> eval xs s
            
genTerms :: [String] -> ([Term], Dict)
genTerms xs = genMachine xs []

main :: IO ()
main = do
    input <- getLine
    let parsed = words input
    let (terms, dict) = genTerms parsed
    putStrLn (show (eval terms []))
    -- putStrLn ((show terms) ++ (show dict))