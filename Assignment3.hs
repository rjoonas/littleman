module Assignment3 where

-- Note to self: Don't add any imports!
import Parsing
import Control.Monad.State

-- Exercise 1a: a combinator that tries to apply parser p and converts its failure to value x.
option :: a -> Parser a -> Parser a
option x p = p +++ return x

-- Exercise 1b: a combinator that uses option to lift a parser to Maybe.
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing $ liftM Just p

-- Exercise 2: Parse as separated by bs.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pat sep = do
  p <- pat
  ps <- many (sep >> pat)
  return (p : ps)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep +++ return []

type Label = String
type Program = [(Maybe Label, Instruction)]

data Instruction = ADD Label | SUB Label | STA Label | LDA Label | BRA Label | BRZ Label | BRP Label | DAT Int | INP | OUT | HLT deriving Eq

-- There seem to be valid ways of doing this more concisely but I can't figure out how to do it without extra extensions or imports. 
instance Show Instruction where
  show (ADD l) = "ADD " ++ l
  show (SUB l) = "SUB " ++ l
  show (STA l) = "STA " ++ l
  show (LDA l) = "LDA " ++ l
  show (BRA l) = "BRA " ++ l
  show (BRZ l) = "BRZ " ++ l
  show (BRP l) = "BRP " ++ l
  show (DAT i) = "DAT " ++ show i
  show INP = "INP"
  show OUT = "OUT"
  show HLT = "HLT"

-- Exercise 3: instruction parser.

-- Why couldn't we just have separate types depending on the instruction parameters and a general coproduct to enable handling them with one type?
type LabelInstruction = Label -> Instruction
type IntInstruction = Int -> Instruction

labelIScanner :: String -> LabelInstruction -> Parser Instruction
labelIScanner s i = symbol s >> many alphanum >>= \lab -> return (i lab)

labelIPattern :: LabelInstruction -> String
labelIPattern instr = head.words.show.instr $ []

labelIParser :: LabelInstruction -> Parser Instruction
labelIParser l = labelIScanner (labelIPattern l) l

labelIParsers :: Parser Instruction
labelIParsers = foldr1 (+++) $ map labelIParser [ADD,SUB,STA,LDA,BRA,BRZ,BRP]

datParser :: Parser Instruction
datParser = symbol "DAT" >> option 0 nat >>= \i -> return (DAT i)

cmdIParser :: Instruction -> Parser Instruction
cmdIParser i = symbol (show i) >> return i

cmdIParsers :: Parser Instruction
cmdIParsers = foldr1 (+++) $ map cmdIParser [INP,OUT,HLT]

instruction :: Parser Instruction
instruction = labelIParsers +++ cmdIParsers +++ datParser

-- Exercise 4: parser combinator for a program line.
labeledLine :: Parser (Maybe Label, Instruction)
labeledLine = do
  l <- upper
  ls <- many alphanum
  i <- instruction
  return (Just (l:ls), i)

unlabeledLine :: Parser (Maybe Label, Instruction)
unlabeledLine = space >> instruction >>= \i -> return (Nothing, i)

lineComment :: Parser ()
lineComment = do
  string "//"
  many $ sat (/= '\n')
  return ()

{-
It would be easy to implement commented-out lines as line = commentLine +++ instructionLine
but this would require changes to types because not every line would parse to an instruction.
-}
line :: Parser (Maybe Label, Instruction)
line = do
  res <- labeledLine +++ unlabeledLine
  space
  option () lineComment
  space
  return res

parseError :: String -> String
parseError rest = let rest' = if head rest == '\n' then tail rest else rest
                  in takeWhile (/= '\n') rest' ++ "\n^"

-- Modified this pre-given function to add extra features
-- * one trailing newline is allowed
-- * for parse errors we get a nice error message showing where in the program the error was.
parseLMC :: String -> Program
parseLMC s = case parse (sepBy line (char '\n')) s of
               [(p, "")] -> p
               [(p, "\n")] -> p
               [(p, r)] -> error $ "Parse error on line " ++ show (length p + 1) ++ ":\n" ++ parseError r

-- Exercise 5: pretty printing for programs.
showProgram :: Program -> String
showProgram p = init $ foldl f "" p where f acc (optLabel, i) = acc ++ maybe " " (++ " ") optLabel ++ show i ++ "\n"

type Addr = Int
type Accumulator = Maybe Int
type PC = Int -- why do we declare this type at all?
type Mailbox = (Label, Int) -- updated per assignment paper, was (String, Int)

data Env = Env
    { mailboxes :: [Mailbox] -- updated per assignment paper, was [(String, Int)]
    , accumulator :: Accumulator
    , pc :: Addr -- program counter
    , instructions :: [Instruction]
    , labelAddr :: [(Label, Addr)] -- updated per assignment paper, was [(String, Int)]
    }

-- Exercise 6: the execution environment.
datToMailbox :: (Maybe Label, Instruction) -> Maybe Mailbox
datToMailbox t = case t of
                    (Just label, DAT v) -> Just (label, v)
                    _ -> Nothing

initMailboxes :: Program -> [Mailbox]
initMailboxes p = [i | Just i <- map datToMailbox p]

initLabelAddr :: [Maybe Label] -> [(Label, Addr)]
initLabelAddr optLabels = [(l, i) | (Just l, i) <- zip optLabels [0..length optLabels-1]]

mkInitEnv :: Program -> Env
mkInitEnv p = Env {
                mailboxes = initMailboxes p,
                accumulator = Nothing,
                pc = 1, -- execution always starts at 0, pc points to next addr
                instructions = map snd p,
                labelAddr = initLabelAddr (map fst p)}

type IOEnv = StateT Env IO

jumpTo :: Label -> IOEnv()
jumpTo l = labelAddress l >>= instructionAt >>= decode

conditionalJump :: Bool -> Label -> IOEnv()
conditionalJump c l = if c then jumpTo l else nextInstruction >>= decode

binaryOp :: Label -> (Int -> Int -> Int) -> IOEnv()
binaryOp l op = do
  box <- getMailbox l
  acc <- getAccumulator
  setAccumulator (op acc (snd box))

-- Exercise 7: LMC evaluation.
decode :: Instruction -> IOEnv ()
decode HLT = return ()
decode (BRA l) = jumpTo l
decode (BRZ l) = getAccumulator >>= \a -> conditionalJump (a==0) l
decode (BRP l) = getAccumulator >>= \a -> conditionalJump (a>0) l
decode i = decodeSeq i >> nextInstruction >>= decode

decodeSeq :: Instruction -> IOEnv ()
decodeSeq OUT = getAccumulator >>= liftIO.print
decodeSeq (STA l) = getAccumulator >>= setMailbox l
decodeSeq (LDA l) = getMailbox l >>= setAccumulator.snd
decodeSeq (DAT i) = liftIO $ print "WARN: execution reached a DAT."
decodeSeq (ADD l) = binaryOp l (+)
decodeSeq (SUB l) = binaryOp l (-)

decodeSeq INP = do
  liftIO $ putStr "Input? "
  inp <- liftIO (readLn :: IO Int)
  setAccumulator inp

setAccumulator :: Int -> IOEnv ()
setAccumulator acc = do
  env <- get
  put $ env { accumulator = Just acc }

getAccumulator :: IOEnv Int
getAccumulator = do
  env <- get
  case accumulator env of
    Just i -> return i
    Nothing -> error "Illegal access to empty accumulator."

labelErr :: Label -> a
labelErr l = error $ "Invalid label: " ++ l ++ "."

labelLookup :: Label -> [(Label, a)] -> (a -> b) -> b
labelLookup l source f = maybe (labelErr l) f (lookup l source)

getMailbox :: Label -> IOEnv Mailbox
getMailbox l = do
  env <- get
  labelLookup l (mailboxes env) (\val -> return (l, val))

labelAddress :: Label -> IOEnv Int
labelAddress l = do
  env <- get
  labelLookup l (labelAddr env) return

setMailbox :: Label -> Int -> IOEnv ()
setMailbox l v = do
  env <- get
  let boxes = mailboxes env
  if l `elem` map fst boxes then
    put $ env { mailboxes = map (\(l', v') -> if l==l' then (l, v) else (l', v')) boxes }
  else labelErr l

nextInstruction :: IOEnv Instruction
nextInstruction = get >>= instructionAt.pc

instructionAt :: Int -> IOEnv Instruction
instructionAt i = do
  env <- get
  when (i >= length (instructions env)) $ error ("Program counter overflow: " ++ show i)
  put $ env { pc = i + 1 }
  return $ instructions env !! i

evalProgram :: Program -> IO ()
evalProgram [] = return ()
evalProgram p@((_,i):_) = liftM fst $ runStateT (decode i) (mkInitEnv p)
