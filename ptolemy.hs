import qualified System.Random as R

wrapWidth :: Int
wrapWidth = 80

lineEnd :: String
lineEnd = "\r"

splitter :: String
splitter = "==========" ++ lineEnd

bookmarkPrefix :: String
bookmarkPrefix = "- Your Bookmark"

prefixLen :: Int
prefixLen = length bookmarkPrefix

highlightPrefixLen :: Int
highlightPrefixLen = length "- Your Highlight on Page "

folder :: String -> ([[String]], [String]) -> ([[String]], [String])
folder line (blocks, currentBlock)
  | line == lineEnd = (blocks, currentBlock)
  | line == splitter = (currentBlock : blocks, [])
  | otherwise = (blocks, currentBlock ++ [line])

predicate :: [String] -> Bool
predicate [] = False
predicate block = take prefixLen (head block) /= bookmarkPrefix

process :: String -> [[String]]
process = filter predicate . fst . foldr folder ([], []) . lines

wrapper :: (String, String) -> String -> (String, String)
wrapper (acc, currentLine) word =
  let newLine = currentLine ++ word ++ " "
  in if length newLine > wrapWidth
       then (acc ++ currentLine ++ "\n", word ++ " ")
       else (acc, newLine)

wrap :: String -> String
wrap = uncurry (++) . foldl wrapper ("", "") . words

main :: IO ()
main = do
  contents <- getContents
  gen <- R.getStdGen
  let entries = process contents
  let [quote, meta, title] = entries !! fst (R.randomR (0, length entries) gen)
  let page = takeWhile (/= ' ') $ drop highlightPrefixLen meta
  putStr $ "\n" ++ wrap quote ++ "\n\n" ++ wrap (title ++ "p" ++ page) ++ "\n\n"
