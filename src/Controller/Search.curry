--------------------------------------------------------------------------
--- This module contains a controller for search modules.
--------------------------------------------------------------------------
{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

module Controller.Search
  ( searchController, searchForm )
 where

import Char ( toLower )
import Database.CDBI.ER 

import Recipes
import System.SessionInfo
import System.Spicey
import View.Recipe
import View.Search

-----------------------------------------------------------------------------
--- Controller for the main page.
searchController :: Controller
searchController = return [formExp searchForm]

searchForm :: HtmlFormDef ()
searchForm = HtmlFormDef "Controller.Search.searchForm" done
  (\_ -> searchPageView searchRecipeNames searchRecipeIngredients)

--- Controller for searching in recipe titles.
searchRecipeNames :: String -> Controller
searchRecipeNames searchstring = do
  sinfo <- getUserSessionInfo
  let pattern = "%" ++ filter (`notElem` "%_") (map toLower searchstring) ++ "%"
  recipes <- runQ $
              ``sql* Select *
                     From Recipe
                     Where Name like {pattern};''
  return (listRecipeView sinfo ("Rezepte mit: " ++ searchstring) recipes)


--- Controller for searching in recipe ingredientss.
searchRecipeIngredients :: String -> Controller
searchRecipeIngredients searchstring = do
  sinfo <- getUserSessionInfo
  let pattern = "%" ++ filter (`notElem` "%_") (map toLower searchstring) ++ "%"
  reckeys <- runQ $
    ``sql* Select rec.Key
           From Recipe As rec, RecipeDescription As rd
           Where rd.RecipeRecDescKey = rec.Key
                 And rd.Ingredients like {pattern};''
  recipes <- runQ $ mapM getRecipe reckeys
  return (listRecipeView sinfo ("Rezepte mit: " ++ searchstring) recipes)


-----------------------------------------------------------------------------
