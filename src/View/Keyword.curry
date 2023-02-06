module View.Keyword
  ( wKeyword, tuple2Keyword, keyword2Tuple, wKeywordType
  , showKeywordView, listKeywordView
  , keywordAlphabetView )
where

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Recipes
import Config.EntityRoutes
import System.SessionInfo
import System.Spicey
import View.EntitiesToHtml

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
showKeywordView :: UserSessionInfo -> Keyword -> [BaseHtml]
showKeywordView _ keyword =
  keywordToDetailsView keyword
   ++ [hrefPrimSmButton "?Keyword/list" [htxt "back to Keyword list"]]

--- Compares two Keyword entities. This order is used in the list view.
leqKeyword :: Keyword -> Keyword -> Bool
leqKeyword x1 x2 = keywordName x1 <= keywordName x2

--- Supplies a list view for a given list of Keyword entities.
listKeywordView :: UserSessionInfo -> String -> [Keyword] -> [BaseHtml]
listKeywordView _ title keywords =
  [h1 [htxt title],
   par (intercalate [nbsp]
          (concatMap keywordToListView (sortBy leqKeyword keywords)))]


--- Supplies a list view for a given list of characters to select keywords
--- starting with this character.
keywordAlphabetView :: [Char] -> [BaseHtml]
keywordAlphabetView cs =
  [h1 [htxt $ "Stichworte beginnend mit..."],
   par (intercalate [nbsp] (map charButton cs)),
   par [hrefPrimSmButton (showControllerURL "Keyword" ["all"])
                         [htxt "Alle Stichworte"]]]
 where
  charButton c =
    [hrefPrimBadge (showControllerURL "Keyword" ["char", string2urlencoded [c]])
                   [htxt [c]]]
