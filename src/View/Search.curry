--------------------------------------------------------------------------
--- This module implements the views related to searching moduls.
--------------------------------------------------------------------------

module View.Search ( searchPageView ) where

import HTML.Base
import HTML.Styles.Bootstrap3

import System.Spicey

-----------------------------------------------------------------------------
--- A view for searching modules.
searchPageView :: UserSessionInfo -> (String -> Controller)
               -> (String -> Controller) -> [HtmlExp]
searchPageView sinfo searchnamecontroller searchingrcontroller =
  [h1 [htxt $ "Suche in allen Rezepten"],
   par [htxt $ "Suche alle Rezepten mit dem Text: ", nbsp,
        textfield scode "" `addAttr` ("size","20")],
   par [primButton "Suche in allen Titeln" searchNameHandler,
        primButton "Suche in den Zutaten" searchIngrHandler]]
 where
  scode free

  searchNameHandler env = searchnamecontroller (env scode) >>= getForm

  searchIngrHandler env = searchingrcontroller (env scode) >>= getForm

-----------------------------------------------------------------------------
