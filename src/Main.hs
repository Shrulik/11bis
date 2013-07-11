-- | You can find a tutorial on this template at https://www.fpcomplete.com/school/tagsoup

module Main where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HTML.TagSoup (Tag, parseTags, sections, (~==), fromAttrib, isTagOpenName)
import Control.Monad (liftM)

-- The URL we're going to search
url = "http://www.bing.com/search?q=school+of+haskell"

-- The tag we're going to search for
dataTag = "<h3>"

-- The function to process the Tags
process :: [[Tag String]] -> [String]
process = map (fromAttrib "href") . filter (isTagOpenName "a") . map (!! 1)

-- The function to find tags. 
findTags :: String -> [[Tag String]]
findTags = sections (~== dataTag) . parseTags

page :: IO String
page = simpleHTTP (getRequest url) >>= getResponseBody

main = (print . process . findTags) =<< page
