import Assignment3
import Parsing
import Control.Monad.State -- Allowed, imported in Assignment3

{-
-- NOTE: Comment these out in the final submission to avoid losing points.

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Data.Maybe

expectParse :: Bool -> FilePath -> Property
expectParse ok filename = monadicIO $ do
  program <- run $ parseFile filename
  assert $ (if ok then not.null else null) program 

shouldParse = expectParse True
shouldNotParse = expectParse False

prop_io = shouldParse "examples/io.lmc"
prop_var = shouldParse "examples/variable.lmc"
prop_errLda = shouldParse "examples/err_lda.lmc"
prop_errSta = shouldParse "examples/err_sta.lmc"
prop_countdown = shouldParse "examples/countdown.lmc" 
prop_fibonacci = shouldParse "examples/fibonacci.lmc"
prop_primes = shouldParse "examples/primes.lmc"

-- Can we read label addresses properly for the fibonacci program?
prop_initLabelAddr :: Property
prop_initLabelAddr = monadicIO $ do
  p <- run $ parseFile "examples/fibonacci.lmc"
  assert $ initLabelAddr (map fst p) == [("START",2),("END",15),("FIRST",16),("SECOND",17),("TEMP",18),("COUNT",19),("ONE",20)]

-- Ghetto REPL, not very useful yet as we can't preload DATs.
runRepl :: Maybe Env -> IO ()
runRepl optEnv = do
  input <- getLine
  let program = parseLMC input ++ replicate 100 (Nothing, HLT)
  let env = fromMaybe (mkInitEnv program) optEnv
  (_, env') <- evalWithEnv env program
  runRepl (Just env')

repl :: IO ()
repl = runRepl Nothing
-}

withFile :: FilePath -> (String -> a) -> IO a
withFile filename f = liftM f (readFile filename)

parseFile :: FilePath -> IO Program
parseFile filename = withFile filename parseLMC

evalFile :: FilePath -> IO ()
evalFile filename = parseFile filename >>= evalProgram

showFile :: FilePath -> IO ()
showFile filename = parseFile filename >>= putStrLn.showProgram

evalWithEnv :: Env -> Program -> IO ((), Env)
evalWithEnv env p@((_,i):_) = runStateT (decode i) env

prop_option :: String -> Bool
prop_option s = parse (option 'f' item) s == if null s then [('f', s)] else [(head s, tail s)]

prop_optionMaybe1 :: Bool
prop_optionMaybe1 = parse (optionMaybe int) "12a" == [(Just 12, "a")]

prop_optionMaybe2 :: Bool
prop_optionMaybe2 = parse (optionMaybe int) "a" == [(Nothing, "a")]

prop_sepBy :: Bool
prop_sepBy = parse (sepBy (char 'a') (char ' ')) "a a b" == [("aa"," b")]

prop_parseAdd :: Bool
prop_parseAdd = parse (labelIParser ADD) "ADD LBL" == [(ADD "LBL", "")]

allParseTo :: Eq a => Parser a -> [(String, a)] -> Bool
allParseTo parser expect = map (parse parser . fst) expect == map (\exp -> [(snd exp, [])]) expect

prop_instruction :: Bool
prop_instruction = allParseTo instruction [
    ("ADD PLUS", ADD "PLUS"),
    ("SUB LABEL", SUB "LABEL"),
    ("STA MBOX", STA "MBOX"),
    ("LDA 100", LDA "100"),
    ("BRA LBL", BRA "LBL"),
    ("BRZ BRAZIL", BRZ "BRAZIL"),
    ("BRP BURP", BRP "BURP"),
    ("INP", INP),
    ("OUT", OUT),
    ("HLT", HLT),
    ("DAT", DAT 0),
    ("DAT 666", DAT 666)]

prop_line :: Bool
prop_line = allParseTo line [
    (" LDA A//cmt", (Nothing, LDA "A")),
    ("     LDA A // comment", (Nothing, LDA "A")),
    ("LABEL LDA A // comment", (Just "LABEL", LDA "A"))
  ]

