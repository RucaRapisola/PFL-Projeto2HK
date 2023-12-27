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

--not working still (factorial run class)
run ((Loop loopCode breakCode):code, stack, state) = loop (loopCode, breakCode, code, stack, state)
  where
    loop (lCode, bCode, restCode, s, st) =
      let (_, conditionStack, _) = run (bCode, s, st) in
      case conditionStack of
        (cond:cs) -> if intToBool cond
                     then run (restCode, cs, st)
                     else let (_, newStack, newState) = run (lCode, s, st) in
                          loop (lCode, bCode, restCode, newStack, newState)
        [] -> error "Condition stack is empty in Loop"


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
type Program = Stm

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
compile (Assign var aexp) = compA aexp ++ [Store var]

parse :: String -> Program
parse = parseStm . myLexer

-- Parser

parseStm :: [String] -> Stm
parseStm (x : ":=" : ys) = Assign x (parseAexp ys)
parseStm _ = error "Invalid statement"

parseAexp :: [String] -> Aexp
parseAexp [x, "+", y] = AddA (parseAexp [x]) (parseAexp [y])
parseAexp [x, "-", y] = SubA (parseAexp [x]) (parseAexp [y])
parseAexp [x, "*", y] = MultA (parseAexp [x]) (parseAexp [y])
parseAexp [x] = if all isDigit x then Num (read x) else Var x
parseAexp _ = error "Invalid expression"


myLexer :: String -> [String]
myLexer = words . map replace
  where
    replace ';' = ' '
    replace c = c

-- Examples:
-- testParser("x := 5") == ("","x=5")
-- testParser("x := 5 - 1") == ("","x=4")



-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")