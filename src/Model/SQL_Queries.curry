{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- Some SQL queries on the recipe database

module Model.SQL_Queries where

import Database.CDBI.ER

import Model.Recipes

-----------------------------------------------------------------------
--- Gets all Keyword entities having a given string as keyword.
queryAKeyword :: String -> DBAction [Keyword]
queryAKeyword kws =
  ``sql* Select *
         From Keyword
         Where Name = {kws};''

queryDescriptionOfRecipe :: RecipeID -> DBAction (Maybe RecipeDescription)
queryDescriptionOfRecipe rkey =
  fmap (\xs -> if null xs then Nothing else Just (head xs))
  ``sql* Select *
         From RecipeDescription As rd
         Where rd.RecipeRecDescKey = {rkey};''

--- Gets the associated Recipe entities for a given Keyword entity
getKeywordRecipes :: Keyword -> DBAction [Recipe]
getKeywordRecipes kw1 = fmap (map fst)
  ``sql* Select *
         From Recipe As rec Inner Join Keyword as kw
                            On Satisfies rec recWithKeyword kw
         Where kw.Key = {keywordKey kw1};''

-----------------------------------------------------------------------
