module View.Keyword
  ( wKeyword, tuple2Keyword, keyword2Tuple, wKeywordType
  , showKeywordView, listKeywordView
  , keywordAlphabetView )
where

import HTML.Base
import HTML.WUI
import Time
import Sort
import HTML.Styles.Bootstrap3
import System.Spicey
import System.SessionInfo
import Recipes
import View.RecipesEntitiesToHtml

--- The WUI specification for the entity type Keyword.
wKeyword :: WuiSpec String
wKeyword = withRendering wRequiredString (renderLabels keywordLabelList)

--- Transformation from data of a WUI form to entity type Keyword.
tuple2Keyword :: Keyword -> String -> Keyword
tuple2Keyword keywordToUpdate name = setKeywordName keywordToUpdate name

--- Transformation from entity type Keyword to a tuple
--- which can be used in WUI specifications.
keyword2Tuple :: Keyword -> String
keyword2Tuple keyword = keywordName keyword

--- WUI Type for editing or creating Keyword entities.
--- Includes fields for associated entities.
wKeywordType :: Keyword -> WuiSpec Keyword
wKeywordType keyword =
  transformWSpec (tuple2Keyword keyword,keyword2Tuple) wKeyword

------------------------------------------------------------------------------
--- Supplies a view to show the details of a Keyword.
showKeywordView :: UserSessionInfo -> Keyword -> [HtmlExp]
showKeywordView _ keyword =
  keywordToDetailsView keyword
   ++ [hrefButton "?Keyword/list" [htxt "back to Keyword list"]]

--- Compares two Keyword entities. This order is used in the list view.
leqKeyword :: Keyword -> Keyword -> Bool
leqKeyword x1 x2 = keywordName x1 <= keywordName x2

--- Supplies a list view for a given list of Keyword entities.
listKeywordView :: UserSessionInfo -> String -> [Keyword] -> [HtmlExp]
listKeywordView _ title keywords =
  [h1 [htxt title]
  ,spTable (map keywordToListView (sortBy leqKeyword keywords))]


--- Supplies a list view for a given list of characters to select keywords
--- starting with this character.
keywordAlphabetView :: [Char] -> [HtmlExp]
keywordAlphabetView cs =
  [h1 [htxt $ "Stichworte beginnend mit..."],
   par (map (\c -> hrefButton (showControllerURL "Keyword"
                                  ["char", string2urlencoded [c]])
                          [htxt [c], nbsp]) cs),
   par [hrefButton (showControllerURL "Keyword" ["all"])
                   [htxt "Alle Stichworte"]]]
