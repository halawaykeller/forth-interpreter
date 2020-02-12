
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

-- Limititations : Can't do nested function defintion

-- Data Types

type Stack = [Term]
type Dict = [(String, [Term])]
type AbstractMachine = (Stack, Dict)

data Term = 
    Const Int 
    | Word String [Term] 
    | PrimOp Op 
    | If [Term] [Term]

data Op = 
      Add (Int -> Int -> Int) 
    | Sub (Int -> Int -> Int)
    | Mult (Int -> Int -> Int)
    | Div (Int -> Int -> Int)
    | Eq (Int -> Int -> Int)
    | Greater (Int -> Int -> Int)
    | Less (Int -> Int -> Int)

instance Show Term where
    show ex = case ex of
        Const e -> " Const " ++ show e
        Word s e -> " Word " ++ s ++ (concatMap show e)
        PrimOp e -> " PrimOp " ++ " <function> " 
        If t1 t2 -> "If " ++ (concatMap show t1) ++ (concatMap show t2)

-- Equality operators

eq :: Int -> Int -> Int
eq x y 
    | x == y = 1
    | otherwise = 0

greater :: Int -> Int -> Int
greater x y 
    | x > y = 1
    | otherwise = 0

less :: Int -> Int -> Int
less x y 
    | x < y = 1
    | otherwise = 0

-- Stack manipulation

push :: Term -> Stack -> Stack
push e stack = e:stack 

peek :: Stack -> Term
peek (e:stack) = e

-- Evaluators

evalPrimOp :: Op -> [Term] -> Term
evalPrimOp _ [] = error("Cannot eval primop on empty list")
evalPrimOp _ ([x]) = error("Cannot eval primop with single argument")
evalPrimOp op ((Const x):(Const y):xs) = case op of 
    Add f -> Const (f x y)
    Sub f -> Const (f y x)
    Mult f -> Const (f x y)
    Div f -> Const (f y x)
    Eq f -> Const (f x y)
    Greater f -> Const (f x y)
    Less f -> Const (f x y)


eval :: Stack -> Stack -> Stack
eval [] s = s
eval (x:xs) s = case x of 
        Const a -> eval xs (push (Const a) s)
        PrimOp f ->  eval xs (push result (drop 2 s))
            where result = evalPrimOp f (take 2 s)
        If t1 t2 -> eval xs (result ++ (drop 1 s))
            where result = if (checkStackForBool (head s)) == 1 then t1 else t2

checkStackForBool :: Term -> Int
checkStackForBool (Const a) = a
checkStackForBool anyVal = error("Only a constant can indicate true / false")

-- Parsing

toTerm :: [String]  -> [Term]
toTerm [] = []
toTerm (x:xs) = case x of
        "+" -> (PrimOp $ Add (+)) : toTerm xs
        "-" -> (PrimOp $ Sub (-)) : toTerm xs
        "*" -> (PrimOp $ Mult (*)) : toTerm xs
        "/" -> (PrimOp $ Div (div)) : toTerm xs
        "=" -> (PrimOp $ Div (eq)) : toTerm xs
        ">" -> (PrimOp $ Div (greater)) : toTerm xs
        "<" -> (PrimOp $ Div (less)) : toTerm xs
        ":" -> wordToTerm xs : toTerm (dropWord xs)
        ";" -> toTerm xs
        "if" -> (If t1 t2) : toTerm (dropWord xs)
                where (t1, t2) = setIf(xs)
        "else" -> toTerm xs
        "then" -> toTerm xs
        _ -> (Const (read x :: Int)) : toTerm xs

wordToTerm :: [String] -> Term
wordToTerm xs = setWord (parseWord xs)

parseWord :: [String] -> [String]
parseWord xs = takeWhile (/= ";") xs

dropWord :: [String] -> [String]
dropWord xs = dropWhile (/= ";") xs

setIf :: [String] -> ([Term], [Term])
setIf xs = (toTerm upToElse, toTerm elseToThen)
    where upToElse = takeWhile (/= "else") xs
          elseToThen = dropWhile (/= "else") (takeWhile (/= "then") xs)

setWord :: [String] -> Term
setWord [] = error("Could not define word for empty list")
setWord [x] = error("Cannot define word without instructions")
setWord (x:xs) = Word x (toTerm xs)

-- Creating the Abstract Machine

genMachine :: [String] -> Dict -> AbstractMachine
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
    ":" -> (terms,  dict)
            where Word name ts = wordToTerm xs 
                  updatedDict = (name, ts):d
                  (terms, dict) = genMachine (dropWord xs) updatedDict
    ";" -> genMachine xs d
    val  -> case (lookup val d) of
                Just ts -> (ts ++ terms, dict)
                    where (terms, dict) = genMachine xs d
                Nothing -> ((Const (read x :: Int)) : terms, dict)
                     where (terms, dict) = genMachine xs d
                
            
abstractMachine :: [String] -> AbstractMachine
abstractMachine xs = genMachine xs []

-- Main

main :: IO ()
main = do
    input <- getLine
    let parsed = words input
    let (terms, dict) = abstractMachine parsed
    putStrLn (show (eval terms []))
