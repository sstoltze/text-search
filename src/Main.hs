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

-- If more than one word, show only lines containing all of them
loopLookupWords :: WordIndex -> IO ()
loopLookupWords index = do
  putStrLn "Enter words to search for (:q to quit):"
  putStr "> "
  line <- getLine
  let ws = T.words $ T.pack line
      -- handleWord word =
      --   if word == ":q"
      --   then putStr ""
      --   else do
      --     putStrLn $ "Search for: " ++ T.unpack word
      --     let w = standardise word
      --     if Map.member w index
      --       then mapM_ (putStrLn . show) $ categoriseEntries $ index Map.! w
      --       else putStrLn "Not found."
      --     putStrLn ""
--  mapM_ handleWord $ ws
  if any (== ":q") ws
    then return ()
    else do
    if all (\x -> Map.member x index) ws
      then let cats = map (categoriseEntries . (index Map.!)) ws
           in mapM_ (mapM_ (\c -> printCategory c >> putStrLn "")) cats
      else putStrLn "Not found.\n"
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
