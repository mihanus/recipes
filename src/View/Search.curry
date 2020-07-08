--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module View.Search ( searchPageView ) where

import HTML.Base
import HTML.Styles.Bootstrap4

import System.Spicey

-----------------------------------------------------------------------------
--- A view for searching modules.
searchPageView :: (String -> Controller) -> (String -> Controller) -> [HtmlExp]
searchPageView searchnamecontroller searchingrcontroller =
  [h1 [htxt $ "Suche in allen Rezepten"],
   par [htxt $ "Suche alle Rezepte mit dem Text: ", nbsp,
        textField scode "" `addAttr` ("size","20")],
   par [primSmButton "Suche in allen Titeln" searchNameHandler,
        primSmButton "Suche in den Zutaten" searchIngrHandler]]
 where
  scode free

  searchNameHandler env = searchnamecontroller (env scode) >>= getPage

  searchIngrHandler env = searchingrcontroller (env scode) >>= getPage

-----------------------------------------------------------------------------
