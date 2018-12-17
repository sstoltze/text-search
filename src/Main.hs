{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified Data.Char as Char
import qualified Data.Text as T

type Wordname   = T.Text
data Position   = Position { lineNumber :: Int, characterNumber :: Int } deriving Show
data IndexEntry = IndexEntry { fileName :: FilePath, position :: Position, containingLine :: T.Text} deriving Show
type WordIndex  = Map.Map Wordname [IndexEntry]

standardise :: Wordname -> Wordname
standardise = T.toLower

main :: IO ()
main = do
  files  <- Dir.listDirectory "."
  ixs    <- mapM buildIndexFromFile files
  let ix = Map.unionsWith (++) ixs
  loopLookupWords ix

loopLookupWords :: WordIndex -> IO ()
loopLookupWords index = do
  putStrLn "Enter words to search for (:q to quit):"
  putStr "> "
  line <- getLine
  let words = T.words $ T.pack line
      inIndex = flip Map.member $ index
  if any (== ":q") words
    then return ()
    else do
    if all inIndex words
      then let cats = map (categoriseEntries . (index Map.!)) words
               reduce [] _ = []
               reduce ((a1,b1,c1,d1):xs) ys = [(a1,b1,c1,d1++d2) | (a2,b2,c2,d2) <- ys, (a2,b2) == (a1,b1)] ++ reduce xs ys
               matchingLines = foldl reduce (cats !! 0) $ tail cats
           in if null matchingLines
              then putStrLn "Could not find a line containg all the words.\n"
              else mapM_ (\c -> printCategory c >> putStrLn "") matchingLines
      else putStrLn $ "Could not find words: " ++ (T.unpack $ T.unwords $ filter (not . inIndex) words) ++ "\n"
    loopLookupWords index

categoriseEntries :: [IndexEntry] -> [(FilePath, Int, T.Text, [Int])]
categoriseEntries xs = categoriseEntries' xs [] [] where
  categoriseEntries' [] ys _ = ys
  categoriseEntries' (x:xs) ys alreadyChecked = let pair = (fileName x, lineNumber $ position x) in
                                                  if elem pair alreadyChecked
                                                  then categoriseEntries' xs (map (\(f,l,line,positions) -> (f,l,line,if (f,l) == pair
                                                                                                                     then (characterNumber $ position x) : positions
                                                                                                                     else positions))
                                                                                    ys) alreadyChecked
                                                  else categoriseEntries' xs ((fileName x, lineNumber $ position x, containingLine x, [characterNumber $ position x]):ys) (pair:alreadyChecked)

printCategory :: (FilePath, Int, T.Text, [Int]) -> IO ()
printCategory (filename, linenumber, line, positions) = putStrLn $ "File: " ++ filename ++ ", line " ++ (show linenumber) ++ ", positions " ++ (show positions) ++ ".\nContaining line: \"" ++ T.unpack (T.strip $ line) ++ "\""

buildIndexFromFile :: FilePath -> IO WordIndex
buildIndexFromFile file = do
  contents <- readFile file
  let index = Map.empty
      l = map T.pack $ lines contents
      addWordToIndex ix word file lineNumber wordNumber line = let entry = IndexEntry file (Position lineNumber wordNumber) line in Map.insertWith (++) (standardise word) [entry] ix
      addLineToIndex ix line file lineNumber = let f (ix, len) w = (addWordToIndex ix w file lineNumber len line, 1+len+(T.length w)) in fst $ foldl f (ix, T.length $ T.takeWhile Char.isSpace line) $ T.words $ T.map (\c -> if Char.isPunctuation c then ' ' else c) line
      f (ix, lineNumber) line = (addLineToIndex ix line file lineNumber, 1+lineNumber)
  return $ fst $ foldl f (index, 0) l
