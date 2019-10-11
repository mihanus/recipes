module View.RecipeDescription
  ( wRecipeDescription, tuple2RecipeDescription, recipeDescription2Tuple
  , wRecipeDescriptionType
  , showRecipeDescriptionView, listRecipeDescriptionView ) where

import HTML.Base
import HTML.WUI
import Time
import Sort
import HTML.Styles.Bootstrap3
import System.Spicey
import System.SessionInfo
import Recipes
import View.RecipesEntitiesToHtml

--- The WUI specification for the entity type RecipeDescription.
--- It also includes fields for associated entities.
wRecipeDescription
  :: [Recipe] -> WuiSpec (String,String,String,String,String,Recipe)
wRecipeDescription recipeList =
  withRendering
   (w6Tuple wRequiredString wRequiredString wRequiredString wString wString
     (wSelect recipeToShortView recipeList))
   (renderLabels recipeDescriptionLabelList)

--- Transformation from data of a WUI form to entity type RecipeDescription.
tuple2RecipeDescription
  :: RecipeDescription
  -> (String,String,String,String,String,Recipe) -> RecipeDescription
tuple2RecipeDescription
    recipeDescriptionToUpdate
    (servings,ingredients,directions,prepTime,cookTime,recipe) =
  setRecipeDescriptionServings
   (setRecipeDescriptionIngredients
     (setRecipeDescriptionDirections
       (setRecipeDescriptionPrepTime
         (setRecipeDescriptionCookTime
           (setRecipeDescriptionRecipeRecDescKey recipeDescriptionToUpdate
             (recipeKey recipe))
           cookTime)
         prepTime)
       directions)
     ingredients)
   servings

--- Transformation from entity type RecipeDescription to a tuple
--- which can be used in WUI specifications.
recipeDescription2Tuple
  :: Recipe
  -> RecipeDescription -> (String,String,String,String,String,Recipe)
recipeDescription2Tuple recipe recipeDescription =
  (recipeDescriptionServings recipeDescription
  ,recipeDescriptionIngredients recipeDescription
  ,recipeDescriptionDirections recipeDescription
  ,recipeDescriptionPrepTime recipeDescription
  ,recipeDescriptionCookTime recipeDescription
  ,recipe)

--- WUI Type for editing or creating RecipeDescription entities.
--- Includes fields for associated entities.
wRecipeDescriptionType
  :: RecipeDescription -> Recipe -> [Recipe] -> WuiSpec RecipeDescription
wRecipeDescriptionType recipeDescription recipe recipeList =
  transformWSpec
   (tuple2RecipeDescription recipeDescription,recipeDescription2Tuple recipe)
   (wRecipeDescription recipeList)

------------------------------------------------------------------------------
--- Supplies a view to show the details of a RecipeDescription.
showRecipeDescriptionView
  :: UserSessionInfo -> RecipeDescription -> Recipe -> [HtmlExp]
showRecipeDescriptionView _ recipeDescription relatedRecipe =
  recipeDescriptionToDetailsView recipeDescription relatedRecipe
   ++ [hrefButton "?RecipeDescription/list"
        [htxt "back to RecipeDescription list"]]

--- Compares two RecipeDescription entities. This order is used in the list view.
leqRecipeDescription :: RecipeDescription -> RecipeDescription -> Bool
leqRecipeDescription x1 x2 =
  (recipeDescriptionServings x1
  ,recipeDescriptionIngredients x1
  ,recipeDescriptionDirections x1
  ,recipeDescriptionPrepTime x1
  ,recipeDescriptionCookTime x1)
   <= (recipeDescriptionServings x2
      ,recipeDescriptionIngredients x2
      ,recipeDescriptionDirections x2
      ,recipeDescriptionPrepTime x2
      ,recipeDescriptionCookTime x2)

--- Supplies a list view for a given list of RecipeDescription entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of RecipeDescription entities.
listRecipeDescriptionView
  :: UserSessionInfo -> [RecipeDescription] -> [HtmlExp]
listRecipeDescriptionView sinfo recipeDescriptions =
  [h1 [htxt "RecipeDescription list"]
  ,spTable
    ([take 5 recipeDescriptionLabelList]
      ++ map listRecipeDescription
          (mergeSortBy leqRecipeDescription recipeDescriptions))]
  where
    listRecipeDescription recipeDescription =
      recipeDescriptionToListView recipeDescription
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefButton
                      ("?RecipeDescription/show/"
                        ++ showRecipeDescriptionKey recipeDescription)
                      [htxt "show"]]
                   ,[hrefButton
                      ("?RecipeDescription/edit/"
                        ++ showRecipeDescriptionKey recipeDescription)
                      [htxt "edit"]]
                   ,[hrefButton
                      ("?RecipeDescription/delete/"
                        ++ showRecipeDescriptionKey recipeDescription)
                      [htxt "delete"]]])