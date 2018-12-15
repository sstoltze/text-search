{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified Data.Char as Char
import qualified Data.Text as T

type Wordname   = T.Text
data Position   = Position { lineNumber :: Int, characterNumber :: Int } deriving Show
data IndexEntry = IndexEntry { fileName :: FilePath, position :: Position, containingLine :: T.Text}
type WordIndex  = Map.Map Wordname [IndexEntry]

instance Show IndexEntry where
  show entry = let ln = show $ lineNumber $ position entry
                   cn = show $ characterNumber $ position entry
               in "File: " ++ fileName entry ++ ", line " ++ ln ++ ", character " ++ cn ++ ". \"" ++ T.unpack (T.strip $ containingLine entry) ++ "\""

standardise :: Wordname -> Wordname
standardise = T.toLower

main :: IO ()
main = do
  files  <- Dir.listDirectory "."
  ixs    <- mapM buildIndexFromFile files
  let ix = Map.unionsWith (++) ixs
  loopLookupWords ix

-- If more than one word, show only lines containing all of them
-- If word appears more than once in the same line, print the line only once and print how many times the word appears in that line (and positions?)
loopLookupWords :: WordIndex -> IO ()
loopLookupWords index = do
  putStrLn "Enter words to search for (:q to quit):"
  line <- getLine
  putStrLn ""
  let w = T.words $ T.pack line
      handleWord word =
        if word == ":q"
        then putStr ""
        else do
          putStrLn $ "Word: " ++ T.unpack word
          if Map.member word index
            then mapM_ (putStrLn . show) $ index Map.! (standardise word)
            else putStrLn "Not found."
          putStrLn ""
  mapM_ handleWord $ w
  if any (== ":q") w
    then return ()
    else loopLookupWords index

buildIndexFromFile :: FilePath -> IO WordIndex
buildIndexFromFile file = do
  contents <- readFile file
  let index = Map.empty
      l = map T.pack $ lines contents
      addWordToIndex ix word file lineNumber wordNumber line = let entry = IndexEntry file (Position lineNumber wordNumber) line in Map.insertWith (++) (standardise word) [entry] ix
      addLineToIndex ix line file lineNumber = let f (ix, len) w = (addWordToIndex ix w file lineNumber len line, 1+len+(T.length w)) in fst $ foldl f (ix, T.length $ T.takeWhile Char.isSpace line) $ T.words $ T.map (\c -> if Char.isPunctuation c then ' ' else c) line
      f (ix, lineNumber) line = (addLineToIndex ix line file lineNumber, 1+lineNumber)
  return $ fst $ foldl f (index, 0) l
