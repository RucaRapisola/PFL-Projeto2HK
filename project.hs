-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

import Data.List (intercalate)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (strip, pack, unpack)
import Data.Char (isDigit, isAlpha)
import Text.Parsec hiding (State, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

-- Definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Stack and State types
type Stack = [Integer]  -- We will use -1 to represent False and -2 for True on the Stack
type State = [(String, Integer)]

-- Helper functions for boolean values
boolToInt :: Bool -> Integer
boolToInt True = -2
boolToInt False = -1

intToBool :: Integer -> Bool
intToBool n = n == -2

boolToString :: Integer -> String
boolToString n = case n of
                    -1 -> "False"
                    -2 -> "True"
                    _  -> show n

-- Create empty stack and state
createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

-- Convert stack to string (for testing purposes)
stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map boolToString stack

-- Convert state to string (for testing purposes)
state2Str :: State -> String
state2Str [] = ""
state2Str st = init $ concatMap (\(var, val) -> var ++ "=" ++ boolToString val ++ ",") st

-- Fetch and Store operations
fetch :: String -> State -> Integer
fetch var state = maybe (error $ "Variable not found: " ++ var) id (lookup var state)

store :: String -> Integer -> State -> State
store var val st = sortBy (comparing fst) $ (var, val) : filter ((var /=) . fst) st

-- The 'run' function
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) = run (code, n:stack, state)
run (Add:code, x:y:stack, state) = run (code, (x + y):stack, state)
run (Mult:code, x:y:stack, state) = run (code, (x * y):stack, state)
run (Sub:code, x:y:stack, state) = run (code, (x - y):stack, state)
run (Tru:code, stack, state) = run (code, boolToInt True:stack, state)
run (Fals:code, stack, state) = run (code, boolToInt False:stack, state)
run (Equ:code, x:y:stack, state) = run (code, boolToInt (x == y):stack, state)
run (Le:code, y:x:stack, state) = run (code, boolToInt (y <= x):stack, state)
run (And:code, x:y:stack, state) = run (code, boolToInt (intToBool x && intToBool y):stack, state)
run (Neg:code, x:stack, state) = run (code, boolToInt (not $ intToBool x):stack, state)
run (Fetch var:code, stack, state) = run (code, fetch var state:stack, state)
run (Store var:code, x:stack, state) = run (code, stack, store var x state)
run (Noop:code, stack, state) = run (code, stack, state)
run ((Branch trueBranch falseBranch):code, x:stack, state) =
  if intToBool x
  then run (trueBranch ++ code, stack, state)
  else run (falseBranch ++ code, stack, state)
run ((Loop condition loopcode):code, stack, state) = run ( condition ++ [Branch (loopcode ++ [Loop condition loopcode]) code] ++ code, stack, state)


run (inst:_, _, _) = error ("Unrecognized instruction: " ++ show inst)


-- Assembler test function
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2


-- Definition of Aexp and Bexp
-- Aexp
data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving Show

-- Bexp
data Bexp = BTrue | BFalse | Eq Aexp Aexp | Leq Aexp Aexp | AndB Bexp Bexp | NotB Bexp deriving Show

data Stm =  Aexp | Bexp | Assign String Aexp | Skip | If Bexp Stm Stm | While Bexp Stm deriving Show

-- Definition of Program
type Program = [Stm]

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var var) = [Fetch var]
compA (AddA a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubA a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultA a1 a2) = compA a1 ++ compA a2 ++ [Mult]


compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (Eq a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (Leq a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (AndB b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (NotB b) = compB b ++ [Neg]


compile :: Program -> Code
compile [] = []
compile (Assign var aexp : stms ) = compA aexp ++ [Store var]  ++ compile stms 

parse :: String -> Program
parse = parseStms . myLexer

-- Parser

parseStms :: [Token] -> Program
parseStms [] = []
parseStms tokens = case tokens of
  (TVar var : TAssign : ts) ->
    case parseSubOrSumOrProdOrIntOrPar ts of
      Just (exp, TSemiColon : rest) -> Assign var exp : parseStms rest
      _ -> error "Invalid syntax"


parseInt :: [Token] -> Maybe (Aexp , [Token])
parseInt (TNum n : xs) = Just (Num n, xs)
parseInt _ = Nothing

parseSubOrSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseSubOrSumOrProdOrIntOrPar xs = case parseSumOrProdOrIntOrPar xs of
  Just (stm, TMinus : rest) -> case parseSubOrSumOrProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (SubA stm stm2, rest2)
    _ -> Nothing
  result -> result

parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseSumOrProdOrIntOrPar xs = case parseProdOrIntOrPar xs of
  Just (stm, TPlus : rest) -> case parseSumOrProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (AddA stm stm2, rest2)
    _ -> Nothing
  result -> result

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseProdOrIntOrPar xs = case parseIntOrPar xs of
  Just (stm, TTimes : rest) -> case parseProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (MultA stm stm2, rest2)
    _ -> Nothing
  result -> result

parseVar :: [Token] -> Maybe (Aexp , [Token])
parseVar (TVar v : xs) = Just (Var v, xs)
parseVar _ = Nothing

parseIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseIntOrPar (TOpenPar : xs) = case parseSubOrSumOrProdOrIntOrPar xs of
  Just (stm, TClosePar : rest) -> Just (stm, rest)
  _ -> Nothing
parseIntOrPar xs = parseInt xs `orElse` parseVar xs

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y


-- Lexer

data Token 
    = TAssign
    | TIf
    | TThen
    | TElse
    | TTrue
    | TFalse
    | TNot
    | TAnd
    | TLe
    | TDo
    | TEqu
    | TPlus
    | TMinus
    | TTimes
    | TWhile
    | TOpenPar
    | TClosePar
    | TSemiColon
    | TVar String
    | TNum Integer
    deriving (Show, Eq)

myLexer :: String -> [Token]
myLexer [] = []
myLexer (x:xs) 
    | x == ' ' = myLexer xs
    | x == ';' = TSemiColon : myLexer xs
    | x == '(' = TOpenPar : myLexer xs
    | x == ')' = TClosePar : myLexer xs
    | x == '+' = TPlus : myLexer xs
    | x == '-' = TMinus : myLexer xs
    | x == '*' = TTimes : myLexer xs
    | x == '=' = TEqu : myLexer xs
    | x == '<' = TLe : myLexer xs
    | x == '!' = TNot : myLexer xs
    | x == ':' && take 1 xs == "=" = TAssign : myLexer (drop 1 xs)
    | x == 'i' && take 5 xs == "fnot " = TIf : TNot : myLexer (drop 5 xs)
    | x == 'i' && take 2 xs == "f " = TIf : myLexer (drop 2 xs)
    | x == 't' && take 4 xs == "hen " = TThen : myLexer (drop 4 xs)
    | x == 'e' && take 4 xs == "lse " = TElse : myLexer (drop 4 xs)
    | x == 't' && take 4 xs == "rue " = TTrue : myLexer (drop 4 xs)
    | x == 'f' && take 5 xs == "alse " = TFalse : myLexer (drop 5 xs)
    | x == 'w' && take 5 xs == "hile " = TWhile : myLexer (drop 5 xs)
    | x == 'd' && take 2 xs == "o" = TDo : myLexer (drop 2 xs)
    | x == 'n' && take 3 xs == "ot " = TNot : myLexer (drop 3 xs)
    | x == 'T' && take 4 xs == "rue " = TTrue : TSemiColon : myLexer (drop 4 xs)
    | x == 'F' && take 5 xs == "alse " = TFalse : TSemiColon : myLexer (drop 5 xs)
    | x == 'a' && take 3 xs == "nd " = TAnd : myLexer (drop 3 xs)
    | isDigit x = TNum (read (x : takeWhile isDigit xs)) : myLexer (dropWhile isDigit xs)
    | isAlpha x = TVar (x : takeWhile isAlpha xs) : myLexer (dropWhile isAlpha xs)
    | otherwise = error ("Unrecognized token: " ++ [x])



-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")