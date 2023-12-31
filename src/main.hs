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
import Debug.Trace

-- Definition of Inst and Code
-- Definition of the Instruction Set
data Inst =
  Push Integer | -- Pushes an integer onto the stack
  Add | -- Adds the top two integers on the stack
  Mult | -- Multiplies the top two integers on the stack
  Sub | -- Subtracts the top integer from the second top integer
  Tru | -- Pushes True (Boolean) onto the stack
  Fals | -- Pushes False (Boolean) onto the stack
  Equ | -- Checks if the top two stack elements are equal
  Le | -- Checks if the second top element is less than or equal to the top element
  And | -- Performs logical AND on the top two Boolean elements
  Neg | -- Negates the top Boolean element
  Fetch String | -- Fetches the value of a variable from the state
  Store String | -- Stores the top stack element into a variable in the state
  Noop | -- No operation
  Branch Code Code | -- Executes one of two code branches based on the top stack condition
  Loop Code Code -- Loops over code based on a condition
  deriving Show
type Code = [Inst]

-- Definition of Stack Elements
data StackElement = IntVal Integer | BoolVal Bool deriving Show -- Stack elements can be either Integer or Boolean

-- Definition of the Stack
type Stack = [StackElement] -- Stack is a list of StackElements

-- Definition of the State
type State = [(String, StackElement)] -- State maps variable names to StackElements

-- Helper function to convert StackElement to String
stackElementToString :: StackElement -> String
stackElementToString (IntVal n) = show n -- Convert integer value to string
stackElementToString (BoolVal b) = show b -- Convert boolean value to string

-- Function to create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Function to create an empty state
createEmptyState :: State
createEmptyState = []

-- Function to convert a stack to a string representation
stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map stackElementToString stack

-- Function to convert a state to a string representation
state2Str :: State -> String
state2Str [] = ""
state2Str st = init $ concatMap (\(var, val) -> var ++ "=" ++ stackElementToString val ++ ",") st

-- Fetch operation: Retrieves the value of a variable from the state
fetch :: String -> State -> StackElement
fetch var state = case lookup var state of
    Just val -> val
    Nothing  -> error "Run-time error"  -- Error if variable not found

-- Store operation: Stores a value in the state
store :: String -> StackElement -> State -> State
store var val st = sortBy (comparing fst) $ (var, val) : filter ((var /=) . fst) st

-- The 'run' function: Executes the instructions
run :: (Code, Stack, State) -> (Code, Stack, State)
-- Below are the cases handling different instructions
run ([], stack, state) = ([], stack, state) -- Base case: no code to run
run ((Push n):code, stack, state) = run (code, IntVal n:stack, state) -- Push operation
run (Add:code, IntVal x:IntVal y:stack, state) = run (code, IntVal (x + y):stack, state) -- Add operation
run (Mult:code, IntVal x:IntVal y:stack, state) = run (code, IntVal (x * y):stack, state) -- Mult operation
run (Sub:code, IntVal x:IntVal y:stack, state) = run (code, IntVal (x - y):stack, state) -- Sub operation
run (Tru:code, stack, state) = run (code, BoolVal True:stack, state) -- Tru operation
run (Fals:code, stack, state) = run (code, BoolVal False:stack, state) -- Fals operation
run (Equ:code, IntVal x:IntVal y:stack, state) = run (code, BoolVal (x == y):stack, state) -- Equ operation for integers
run (Equ:code, BoolVal x:BoolVal y:stack, state) = run (code, BoolVal (x == y):stack, state) -- Equ operation for booleans
run (Le:code, IntVal x:IntVal y:stack, state) = run (code, BoolVal (y <= x):stack, state) -- Le operation
run (And:code, x:y:stack, state) = case (x, y) of -- And operation
    (BoolVal xb, BoolVal yb) -> run (code, BoolVal (xb && yb):stack, state)
    _ -> error "Run-time error" -- Error if at least one values not boolean
run (Neg:code, BoolVal x:stack, state) = run (code, BoolVal (not x):stack, state) -- Neg operation
run (Fetch var:code, stack, state) = run (code, (fetch var state):stack, state) -- Fetch operation
run (Store var:code, val:stack, state) = run (code, stack, store var val state) -- Store operation
run (Noop:code, stack, state) = run (code, stack, state) -- Noop operation
run ((Branch trueBranch falseBranch):code, BoolVal x:stack, state) = -- Branch operation
  if x
  then run (trueBranch ++ code, stack, state)
  else run (falseBranch ++ code, stack, state)
run ((Loop condition loopcode):code, stack, state) = run (condition ++ [Branch (loopcode ++ [Loop condition loopcode]) code] ++ code, stack, state) -- Loop operation

run (inst:_, _, _) = error ("Unrecognized instruction or invalid types: " ++ show inst) -- Error for unrecognized instruction

-- Assembler test function: Tests a piece of code and returns the final stack and state
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

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
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


-- Part 2


-- Definition of Aexp and Bexp
-- Aexp
data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving Show

-- Bexp
data Bexp = BTrue | BFalse | EqA Aexp Aexp | EqB Bexp Bexp | Leq Aexp Aexp | AndB Bexp Bexp | NotB Bexp deriving Show

data Stm =  Aexp | Bexp | Assign String Aexp | Skip | If Bexp [Stm] [Stm] | While Bexp [Stm] deriving Show

-- Definition of Program
type Program = [Stm]

-- Function to compile an Aexp to Code
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var var) = [Fetch var]
compA (AddA a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubA a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultA a1 a2) = compA a1 ++ compA a2 ++ [Mult]


-- Function to compile a Bexp to Code
compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (EqA a1 a2) = compA a1 ++ compA a2 ++ [Equ]
compB (EqB b1 b2) = compB b1 ++ compB b2 ++ [Equ]
compB (Leq a1 a2) = compA a1 ++ compA a2 ++ [Le]
compB (AndB b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (NotB b) = compB b ++ [Neg]


-- Function to compile a Program to Code, using the functions compA and compB
compile :: Program -> Code
compile [] = []
compile (Assign var aexp : stms ) = compA aexp ++ [Store var]  ++ compile stms 
compile (Skip : stms) = compile stms
compile (If bexp stms1 stms2 : stms) = compB bexp ++ [Branch (compile stms1) (compile stms2)] ++ compile stms
compile (While bexp stms1 : stms) = compB bexp ++ [Branch (compile stms1 ++ [Loop (compB bexp) (compile stms1)]) []] ++ compile stms


-- Function to parse a string to a Program, after tokenizing it
parse :: String -> Program
parse str = case parseStms (myLexer str) of
  (stms, []) -> stms
  _ -> error "Invalid syntax"

-- Parser

-- function to parse a list of statements into a Program and the remaining tokens
parseStms :: [Token] -> (Program, [Token])
parseStms [] = ([], [])
parseStms tokens = case tokens of
  (TVar var : TAssign : ts) ->
    case parseSubOrSumOrProdOrIntOrPar ts of
      Just (exp, TSemiColon : rest) -> 
        let (stms, restTokens) = parseStms rest
        in (Assign var exp : stms, restTokens)
      _ -> error "Invalid syntax assign"
  (TIf : ts ) ->
    case parseLeqOrEqOrTrueOrFalseOrParOrAexp ts of
      Just (exp, TThen : TOpenPar : rest) -> 
        let (stms1, TClosePar : restTokens1) = parseStms rest
        in case restTokens1 of
          (TElse : TOpenPar : rest2) -> 
            let (stms2, TClosePar : TSemiColon : restTokens2) = parseStms rest2
            in (If exp stms1 stms2 : fst (parseStms restTokens2), snd (parseStms restTokens2))
          (TElse : rest2) -> 
            let (stm2, restTokens2) = parseStm rest2
            in (If exp stms1 [stm2] : fst (parseStms restTokens2), snd (parseStms restTokens2))
          _ -> error "Invalid syntax else"
      Just (exp, TThen : rest) -> 
        let (stm1, restTokens1) = parseStm rest
        in case restTokens1 of
          (TElse : TOpenPar : rest2) -> 
            let (stms2, TClosePar : TSemiColon : restTokens2) = parseStms rest2
            in (If exp [stm1] stms2 : fst (parseStms restTokens2), snd (parseStms restTokens2))
          (TElse : rest2) -> 
            let (stm2, restTokens2) = parseStm rest2
            in (If exp [stm1] [stm2] : fst (parseStms restTokens2), snd (parseStms restTokens2))
          _ -> error "Invalid syntax else"
      _ -> error "Invalid syntax if"
  (TWhile : ts) ->
    case parseLeqOrEqOrTrueOrFalseOrParOrAexp ts of
      Just (exp, TDo : TOpenPar : rest) -> 
        let (stms1, TClosePar : TSemiColon : restTokens1) = parseStms rest
        in (While exp stms1 : fst (parseStms restTokens1), snd (parseStms restTokens1))
      _ -> error $ "Invalid syntax while" ++ show ts
  _ -> ([], tokens)

-- function to parse a statement into a Stm and the remaining tokens
parseStm :: [Token] -> (Stm, [Token])
parseStm (TVar var : TAssign : ts) =
  case parseSubOrSumOrProdOrIntOrPar ts of
    Just (exp, TSemiColon : rest) -> (Assign var exp, rest)
    _ -> error "Invalid syntax assign"
parseStm (TIf : ts ) =
  case parseLeqOrEqOrTrueOrFalseOrParOrAexp ts of
    Just (exp, TThen : rest) -> 
      let (stms1, restTokens1) = parseStms rest
      in case restTokens1 of
        (TElse : TOpenPar : rest2) -> 
          let (stms2, TClosePar : restTokens2) = parseStms rest2
          in (If exp stms1 stms2, restTokens2)
        (TElse : rest2) -> 
          let (stm2, restTokens2) = parseStm rest2
          in (If exp stms1 [stm2], restTokens2)
        _ -> error "Invalid syntax else"
    _ -> error "Invalid syntax if"
parseStm (TWhile : ts) =
  case parseLeqOrEqOrTrueOrFalseOrParOrAexp ts of
    Just (exp, TDo : rest) -> 
      let (stms1, restTokens1) = parseStms rest
      in (While exp stms1, restTokens1)
    _ -> error "Invalid syntax while"
parseStm _ = error "Invalid syntax stm"


-- Parser Aexp

-- function to parse a list of tokens into a Num and the remaining tokens.
parseInt :: [Token] -> Maybe (Aexp , [Token])
parseInt (TNum n : xs) = Just (Num n, xs)
parseInt _ = Nothing

-- function to parse a list of tokens into an Aexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Aexp ::= Aexp + Aexp | Aexp - Aexp | Aexp * Aexp | (Aexp) | Var | Int
parseSubOrSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseSubOrSumOrProdOrIntOrPar xs = case parseSumOrProdOrIntOrPar xs of
  Just (stm, TMinus : rest) -> case parseSubOrSumOrProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (SubA stm stm2, rest2)
    _ -> Nothing
  result -> result

-- function to parse a list of tokens into an Aexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Aexp ::= Aexp + Aexp | Aexp * Aexp | (Aexp) | Var | Int
parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseSumOrProdOrIntOrPar xs = case parseProdOrIntOrPar xs of
  Just (stm, TPlus : rest) -> case parseSumOrProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (AddA stm stm2, rest2)
    _ -> Nothing
  result -> result

-- function to parse a list of tokens into an Aexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Aexp ::= Aexp * Aexp | (Aexp) | Var | Int
parseProdOrIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseProdOrIntOrPar xs = case parseIntOrPar xs of
  Just (stm, TTimes : rest) -> case parseProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (MultA stm stm2, rest2)
    _ -> Nothing
  result -> result

-- function to parse a list of tokens into a Var and the remaining tokens.
parseVar :: [Token] -> Maybe (Aexp , [Token])
parseVar (TVar v : xs) = Just (Var v, xs)
parseVar _ = Nothing

-- function to parse a list of tokens into an Aexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Aexp ::= (Aexp) | Var | Int
parseIntOrPar :: [Token] -> Maybe (Aexp , [Token])
parseIntOrPar (TOpenPar : xs) = case parseSubOrSumOrProdOrIntOrPar xs of
  Just (stm, TClosePar : rest) -> Just (stm, rest)
  _ -> Nothing
parseIntOrPar xs = parseInt xs `orElse` parseVar xs

-- function to define an orElse operator for Maybe
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

-- Parser Bexp

-- function to parse a list of tokens into a Bexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Bexp ::= Bexp = Bexp | Bexp <= Bexp | Bexp and Bexp | not Bexp | true | false | (Bexp) | Aexp
parseLeqOrEqOrTrueOrFalseOrParOrAexp :: [Token] -> Maybe (Bexp , [Token])
parseLeqOrEqOrTrueOrFalseOrParOrAexp (TOpenPar : xs) = case parseEqBoolOrAnd xs of
  Just (stm, TClosePar : rest) -> Just (stm, rest)
  _ -> Nothing
parseLeqOrEqOrTrueOrFalseOrParOrAexp (TTrue : xs) = Just (BTrue, xs)
parseLeqOrEqOrTrueOrFalseOrParOrAexp (TFalse : xs) = Just (BFalse, xs)
parseLeqOrEqOrTrueOrFalseOrParOrAexp xs = case parseSubOrSumOrProdOrIntOrPar xs of
  Just (stm, TEqu : rest) -> case parseSubOrSumOrProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (EqA stm stm2, rest2)
    _ -> Nothing
  Just (stm, TLe : rest) -> case parseSubOrSumOrProdOrIntOrPar rest of
    Just (stm2, rest2) -> Just (Leq stm stm2, rest2)
    _ -> Nothing
  result -> Nothing

parseEqBoolOrAnd :: [Token] -> Maybe (Bexp , [Token])
parseEqBoolOrAnd xs = case parseEqBoolOrNot xs of
  Just (stm, TAnd : rest) -> case parseEqBoolOrAnd rest of
    Just (stm2, rest2) -> Just (AndB stm stm2, rest2)
    _ -> Nothing
  result -> result

-- function to parse a list of tokens into a Bexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Bexp ::= Bexp = Bexp | Bexp and Bexp | not Bexp | true | false | (Bexp) | Aexp
parseEqBoolOrNot :: [Token] -> Maybe (Bexp , [Token])
parseEqBoolOrNot xs = case parseNotOrLeqOrEq xs of
  Just (stm, TBoolEqu : rest) -> case parseEqBoolOrNot rest of
    Just (stm2, rest2) -> Just (EqB stm stm2, rest2)
    _ -> Nothing
  result -> result

-- function to parse a list of tokens into a Bexp and the remaining tokens. It parses the following grammar, following the precedence rules:
-- Bexp ::= Bexp and Bexp | not Bexp | true | false | (Bexp) | Aexp
parseNotOrLeqOrEq :: [Token] -> Maybe (Bexp , [Token])
parseNotOrLeqOrEq (TNot : xs) = case parseLeqOrEqOrTrueOrFalseOrParOrAexp xs of
  Just (stm, rest) -> Just (NotB stm, rest)
  _ -> Nothing
parseNotOrLeqOrEq xs = parseLeqOrEqOrTrueOrFalseOrParOrAexp xs




-- Lexer

-- Definition of Token
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
    | TBoolEqu
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

-- Function to tokenize a string
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
    | x == '=' && take 1 xs == "=" = TEqu : myLexer (drop 1 xs)
    | x == '=' = TBoolEqu : myLexer xs
    | x == '<' && take 1 xs == "=" = TLe : myLexer (drop 1 xs)
    | x == ':' && take 1 xs == "=" = TAssign : myLexer (drop 1 xs)
    | x == 'i' && take 1 xs == "f" = TIf : myLexer (drop 1 xs)
    | x == 't' && take 3 xs == "hen" = TThen : myLexer (drop 3 xs)
    | x == 'e' && take 3 xs == "lse" = TElse : myLexer (drop 3 xs)
    | x == 'w' && take 4 xs == "hile" = TWhile : myLexer (drop 4 xs)
    | x == 'd' && take 1 xs == "o" = TDo : myLexer (drop 1 xs)
    | x == 'n' && take 2 xs == "ot" = TNot : myLexer (drop 2 xs)
    | x == 'T' && take 3 xs == "rue" = TTrue : myLexer (drop 3 xs)
    | x == 'F' && take 4 xs == "alse" = TFalse : myLexer (drop 4 xs)
    | x == 'a' && take 2 xs == "nd" = TAnd : myLexer (drop 2 xs)
    | isDigit x = TNum (read (x : takeWhile isDigit xs)) : myLexer (dropWhile isDigit xs)
    | isAlpha x = TVar (x : takeWhile isAlpha xs) : myLexer (dropWhile isAlpha xs)
    | otherwise = error ("Unrecognized token: " ++ [x])



-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4") -> OK
-- testParser "x := 0 - 2;" == ("","x=-2") -> OK
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x := 1; else y := 2;" == ("","y=2") -> OK
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1") -> OK
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")  -> OK
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4") ->  OK
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68") -> OK
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34") -> OK
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1") -> OK
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2") -> OK
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6") -> OK
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1") -> OK
-- testParser "x := 5; y := x * 2;" == ("","x=5,y=10") -> OK
-- testParser "x := 1; y := 2; z := 3; w := x + y + z;" == ("","x=1,y=2,z=3,w=6") -> OK
-- testParser "x := 5; if (x == 5) then y := 1; else y := 2;" == ("","x=5,y=1") -> OK
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2;);" == ("","x=0,y=0") -> OK
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2; if (y == 0) then z := 1; else z := 0;);" == ("","x=0,y=0,z=1") -> OK
-- testParser "x := 5; y := z * 2;" -- Error: 'z' is not defined
-- testParser "x := 1; y := 2; z := 3; w := x + y + z; v := w * 2; u := v / 2; if (u == w) then t := 1; else t := 0; if (v == u) then s := 1;" -- Error: 'else' clause missing for 'if' statement
-- testParser "x := 5; y := x * 2; z := y + 3; w := z / 2; if (w == 6.5) then v := 1; else v := 0; u := v / 2;" -- Error: '/' operator not supported
-- testParser "x := 1; y := 2; z := 3; if (x == 1) then (w := x + y + z; v := w * 2; if (v == 12) then u := 1; else u := 0;) else (t := v / 2;);" -- Error: '/' operator not supported
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2; if (y == 0) then z := 1; else z := 0;); a := b;" -- Error: 'b' is not defined
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2; if (y == 0) then z := 1; else z := 0;); a := b / 2;" -- Error: 'b' is not defined and '/' operator not supported
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2; if (y == 0) then z := 1; else z := 0;); a := b * 2;" -- Error: 'b' is not defined
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2; if (y == 0) then z := 1; else z := 0;); a := b + 2;" -- Error: 'b' is not defined
-- testParser "x := 10; while (not(x == 0)) do (x := x - 1; y := x * 2; if (y == 0) then z := 1; else z := 0;); a := b - 2;" -- Error: 'b' is not defined
-- testParser "if (not(not True)) then x := 1; else y := 2;" == ("","x=1") -> OK

