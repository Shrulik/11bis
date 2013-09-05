-- | You can find a tutorial on this template at https://www.fpcomplete.com/school/tagsoup

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Prelude hiding (concat, putStrLn)
import Data.Text (concat)
import Data.Text.IO (putStrLn)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, element, fromDocument, ($//), (&//), (&/), (&|))

-- The URL we're going to search
url = "http://www.bing.com/search?q=school+of+haskell"

-- The data we're going to search for
findNodes :: Cursor -> [Cursor]
findNodes = element "h3" &/ element "a"

-- Extract the data from each node in turn
extractData = concat . attribute "href"

-- Process the list of data elements
processData = mapM_ putStrLn

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

main = do
     cursor <- cursorFor url 
     processData $ cursor $// findNodes &| extractData