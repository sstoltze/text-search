{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified System.Directory as Dir
import qualified Data.Char as Char
import qualified Data.Text as T

data Position   = Position   { lineNumber      :: Int,
                               characterNumber :: Int }   deriving Show
data IndexEntry = IndexEntry { fileName        :: FilePath,
                               position        :: Position,
                               containingLine  :: T.Text} deriving Show
type WordIndex  = Map.Map T.Text [IndexEntry]

-- Standardise words to a common form
standardise :: T.Text -> T.Text
standardise = T.toLower

main :: IO ()
main = Dir.getCurrentDirectory
       -- Find all files in current directory and subdirectories
       >>= findFilesRecursively
       -- Replace each file with an index
       >>= mapM buildIndexFromFile
       -- Combine indices and allow user to search for words
       >>= loopLookupWords . Map.unionsWith (++)

-- Find files in directory, skipping anything starting with a '.'
findFilesRecursively :: FilePath -> IO [FilePath]
findFilesRecursively dir = do
  isFile      <- Dir.doesFileExist dir
  isDirectory <- Dir.doesDirectoryExist dir
  if isFile
  then return [dir]
  else if isDirectory
       then Dir.listDirectory dir
                >>= (\l -> mapM findFilesRecursively $
                          map (\f -> dir ++ "/" ++ f) $
                              filter (\f -> not (f !! 0 == '.')) l)
                >>= return . concat
       else return []

-- Ask user for words to search for
-- If more than one word is entered at a time, search for lines containing all words
loopLookupWords :: WordIndex -> IO ()
loopLookupWords index = do
  putStrLn "Enter words to search for (:q to quit):"
  putStr   "> "
  line <- getLine
  let words   = T.words $ T.pack line
      inIndex = flip Map.member $ index
  if any (== ":q") words
  then return ()
  else do
    if all inIndex words
    then let cats = map (categoriseEntries . (index Map.!)) words
             -- reduce combines two entries if they have the same file and line number
             reduce [] _ = []
             reduce ((a1,b1,c1,d1):xs) ys = [(a1,b1,c1,d1++d2) | (a2,b2,c2,d2) <- ys, (a2,b2) == (a1,b1)] ++ reduce xs ys
             matchingLines = foldl reduce (cats !! 0) $ tail cats
         in if null matchingLines
            then putStrLn "Could not find a line containg all the words.\n"
            else mapM_ (\c -> printCategory c >> putStrLn "") matchingLines
    else putStrLn $ "Could not find words: " ++ (T.unpack $ T.unwords $ filter (not . inIndex) words) ++ "\n"
    loopLookupWords index

-- Replace a list of IndexEntries with a list of (file, line number, line, list of character positions) tuples, by combining entries that appear in the same line
categoriseEntries :: [IndexEntry] -> [(FilePath, Int, T.Text, [Int])]
categoriseEntries xs = categoriseEntries' xs [] [] where
  categoriseEntries' []     ys _              = ys
  categoriseEntries' (x:xs) ys alreadyChecked =
      let pair = (fileName x, lineNumber $ position x)
      in if elem pair alreadyChecked
         then categoriseEntries' xs (map (\(f,l,line,positions) -> (f,
                                                                   l,
                                                                   line,
                                                                   if (f,l) == pair
                                                                   then (characterNumber $ position x) : positions
                                                                   else positions))

                                     ys) alreadyChecked
       else categoriseEntries' xs ((fileName x,
                                    lineNumber $ position x,
                                    containingLine x,
                                    [characterNumber $ position x]):ys) (pair:alreadyChecked)

-- Print a tuple in a readable fashion
printCategory :: (FilePath, Int, T.Text, [Int]) -> IO ()
printCategory (filename, linenumber, line, positions) = putStrLn $ "File: " ++ filename ++ ", line " ++ (show linenumber) ++ ", positions " ++ (show positions) ++ ".\nContaining line: \"" ++ T.unpack (T.strip $ line) ++ "\""

-- Build an index by adding lines word by word while keeping track of positions using folds
buildIndexFromFile :: FilePath -> IO WordIndex
buildIndexFromFile file = do
  contents <- readFile file
  let index = Map.empty
      ls    = map T.pack $ lines contents
      -- Insert by adding to already existing entry in the index if possible
      addWordToIndex ix word file lineNumber wordNumber line =
          let entry = IndexEntry file (Position lineNumber wordNumber) line
          in Map.insertWith (++) (standardise word) [entry] ix
      addLineToIndex ix line file lineNumber =
          let f (ix, len) w = (addWordToIndex ix w file lineNumber len line, 1+len+(T.length w))
          in fst $
             foldl f (ix, T.length $ T.takeWhile Char.isSpace line) $
             T.words $
             T.map (\c -> if Char.isPunctuation c
                         then ' '
                         else c) line
      f (ix, lineNumber) line = (addLineToIndex ix line file lineNumber, 1+lineNumber)
  return $ fst $ foldl f (index, 0) ls
