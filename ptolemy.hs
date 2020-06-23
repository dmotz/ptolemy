import Prelude hiding (getContents)
import Data.ByteString (getContents)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Random (randomRIO)

main :: IO ()
main = do
  contents <- getContents
  let entries = process $ unpack $ decodeUtf8With lenientDecode contents
  if entries == []
    then putStrLn "no entries found"
    else do
      index <- randomRIO (0, pred $ length entries)
      let [quote, meta, title] = entries !! index
      putStrLn $
        concat
          [ nl
          , wrap quote
          , nl
          , nl
          , wrap $
            concat
              [title, "p", takeWhile (/= ' ') $ drop highlightPrefixLen meta]
          , nl
          ]

folder :: String -> ([[String]], [String]) -> ([[String]], [String])
folder line (blocks, currentBlock)
  | line == lineEnd = (blocks, currentBlock)
  | line == splitter = (currentBlock : blocks, [])
  | otherwise = (blocks, currentBlock ++ [line])

predicate :: [String] -> Bool
predicate [] = False
predicate (x:_) = take prefixLen x /= bookmarkPrefix

process :: String -> [[String]]
process = filter predicate . fst . foldr folder ([], []) . lines

wrapper :: (String, String) -> String -> (String, String)
wrapper (acc, currentLine) word =
  let newLine = concat [currentLine, word, " "]
  in if length newLine > wrapWidth
       then (concat [acc, currentLine, nl], word ++ " ")
       else (acc, newLine)

wrap :: String -> String
wrap = uncurry (++) . foldl wrapper ([], []) . words

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

nl :: String
nl = "\n"
