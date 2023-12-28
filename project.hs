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
compile (Assign var aexp : stms) = compA aexp ++ [Store var] ++ compile stms
compile (If bexp trueBranch falseBranch : stms) = compB bexp ++ [Branch (compile [trueBranch]) (compile [falseBranch])] ++ compile stms

parse :: String -> Program
parse = map parseStm . myLexer

-- Parser

parseStm :: [String] -> Stm
parseStm (x : ":=" : ys) = Assign x (parseAexp ys)
parseStm ("if" : rest ) = If (parseBexp condition) (parseStm trueBranch) (parseStm falseBranch)
  where
    (condition, trueBranch, falseBranch) = parseIf rest
parseStm xs = error $ "Invalid statement: " ++ unwords xs

parseIf :: [String] -> ([String], [String], [String])
parseIf xs = (condition, trueBranch, falseBranch)
  where
    (condition, rest) = parseCondition xs
    (trueBranch, falseBranch) = parseBranch rest

parseCondition :: [String] -> ([String], [String])
parseCondition xs = (condition, tail rest) 
  where
    (condition, rest) = break (== "then") xs

parseBranch :: [String] -> ([String], [String])
parseBranch xs = (trueBranch, tail rest) 
  where
    (trueBranch, rest) = break (== "else") xs



parseAexp :: [String] -> Aexp
parseAexp [x, "+", y] = AddA (parseAexp [x]) (parseAexp [y])
parseAexp [x, "-", y] = SubA (parseAexp [x]) (parseAexp [y])
parseAexp [x, "*", y] = MultA (parseAexp [x]) (parseAexp [y])
parseAexp [x] = if all isDigit x then Num (read x) else Var x
parseAexp _ = error "Invalid expression"


parseBexp :: [String] -> Bexp
parseBexp ["not", x] = NotB (parseBexp [x])
parseBexp (x : "==" : y : xs) = Eq (parseAexp [x]) (parseAexp [y])
parseBexp (x : "<=" : y : xs) = Leq (parseAexp [x]) (parseAexp [y])
parseBexp (x : "and" : y : xs) = AndB (parseBexp [x]) (parseBexp [y])
parseBexp ["True"] = BTrue
parseBexp ["False"] = BFalse
parseBexp xs = error $ "Invalid bool expression: " ++ unwords xs

-- Lexer

myLexer :: String -> [[String]]
myLexer s = case break (== ';') s of
    (left, ';':right) -> if null (words left) then myLexer right else words left : myLexer right
    (left, _) -> if null (words left) then [] else [words left]



-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4") -> funciona
-- testParser "if True then x := 1 else x := 2" == ("","x=1") -> funciona
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")