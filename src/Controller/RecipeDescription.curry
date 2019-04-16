module Controller.RecipeDescription ( mainRecipeDescriptionController ) where

import System.Spicey
import HTML.Base
import Time
import Recipes
import View.RecipeDescription
import Maybe
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import View.RecipesEntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a RecipeDescription entity according to the URL parameter.
mainRecipeDescriptionController :: Controller
mainRecipeDescriptionController =
  do args <- getControllerParams
     case args of
       [] -> listRecipeDescriptionController
       ["list"] -> listRecipeDescriptionController
       ["new"] -> newRecipeDescriptionController
       ["show",s] ->
         applyControllerOn (readRecipeDescriptionKey s)
          (runJustT . getRecipeDescription)
          showRecipeDescriptionController
       ["edit",s] ->
         applyControllerOn (readRecipeDescriptionKey s)
          (runJustT . getRecipeDescription)
          editRecipeDescriptionController
       ["delete",s] ->
         applyControllerOn (readRecipeDescriptionKey s)
          (runJustT . getRecipeDescription)
          deleteRecipeDescriptionController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new RecipeDescription entity.
newRecipeDescriptionController :: Controller
newRecipeDescriptionController =
  checkAuthorization (recipeDescriptionOperationAllowed NewEntity)
   $ (\sinfo ->
     do allRecipes <- runQ queryAllRecipes
        return
         (blankRecipeDescriptionView sinfo allRecipes
           (\entity ->
             transactionController (runT (createRecipeDescriptionT entity))
              (nextInProcessOr listRecipeDescriptionController Nothing))
           listRecipeDescriptionController))

--- Transaction to persist a new RecipeDescription entity to the database.
createRecipeDescriptionT
  :: (String,String,String,String,String,Recipe) -> DBAction ()
createRecipeDescriptionT
    (servings,ingredients,directions,prepTime,cookTime,recipe) =
  newRecipeDescriptionWithRecipeRecDescKey servings ingredients directions
   prepTime
   cookTime
   (recipeKey recipe)
   >+= (\_ -> return ())

--- Shows a form to edit the given RecipeDescription entity.
editRecipeDescriptionController :: RecipeDescription -> Controller
editRecipeDescriptionController recipeDescriptionToEdit =
  checkAuthorization
   (recipeDescriptionOperationAllowed (UpdateEntity recipeDescriptionToEdit))
   $ (\sinfo ->
     do allRecipes <- runQ queryAllRecipes
        recDescRecipe <- runJustT (getRecDescRecipe recipeDescriptionToEdit)
        return
         (editRecipeDescriptionView sinfo recipeDescriptionToEdit
           recDescRecipe
           allRecipes
           (\entity ->
             transactionController (runT (updateRecipeDescriptionT entity))
              (nextInProcessOr listRecipeDescriptionController Nothing))
           listRecipeDescriptionController))

--- Transaction to persist modifications of a given RecipeDescription entity
--- to the database.
updateRecipeDescriptionT :: RecipeDescription -> DBAction ()
updateRecipeDescriptionT recipeDescription =
  updateRecipeDescription recipeDescription

--- Deletes a given RecipeDescription entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteRecipeDescriptionController :: RecipeDescription -> Controller
deleteRecipeDescriptionController recipeDescription =
  checkAuthorization
   (recipeDescriptionOperationAllowed (DeleteEntity recipeDescription))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Really delete entity \""
            ,recipeDescriptionToShortView recipeDescription
            ,"\"?"])]]
      (transactionController
        (runT (deleteRecipeDescriptionT recipeDescription))
        listRecipeDescriptionController)
      (showRecipeDescriptionController recipeDescription))

--- Transaction to delete a given RecipeDescription entity.
deleteRecipeDescriptionT :: RecipeDescription -> DBAction ()
deleteRecipeDescriptionT recipeDescription =
  deleteRecipeDescription recipeDescription

--- Lists all RecipeDescription entities with buttons to show, delete,
--- or edit an entity.
listRecipeDescriptionController :: Controller
listRecipeDescriptionController =
  checkAuthorization (recipeDescriptionOperationAllowed ListEntities)
   $ (\sinfo ->
     do recipeDescriptions <- runQ queryAllRecipeDescriptions
        return (listRecipeDescriptionView sinfo recipeDescriptions))

--- Shows a RecipeDescription entity.
showRecipeDescriptionController :: RecipeDescription -> Controller
showRecipeDescriptionController recipeDescription =
  checkAuthorization
   (recipeDescriptionOperationAllowed (ShowEntity recipeDescription))
   $ (\sinfo ->
     do recDescRecipe <- runJustT (getRecDescRecipe recipeDescription)
        return
         (showRecipeDescriptionView sinfo recipeDescription recDescRecipe))

--- Gets the associated Recipe entity for a given RecipeDescription entity.
getRecDescRecipe :: RecipeDescription -> DBAction Recipe
getRecDescRecipe rRecipe =
  getRecipe (recipeDescriptionRecipeRecDescKey rRecipe)