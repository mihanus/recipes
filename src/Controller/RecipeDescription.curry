module Controller.RecipeDescription ( mainRecipeDescriptionController ) where

import Global
import Maybe
import Time

import HTML.Base
import HTML.Session
import HTML.WUI

import Recipes
import Config.EntityRoutes
import Config.Storage
import Config.UserProcesses
import View.RecipeDescription
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import View.EntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a RecipeDescription entity according to the URL parameter.
mainRecipeDescriptionController :: Controller
mainRecipeDescriptionController =
  do args <- getControllerParams
     case args of
       [] -> listRecipeDescriptionController
       ["list"] -> listRecipeDescriptionController
       ["new"] -> newRecipeDescriptionController
       ["show",s] -> controllerOnKey s showRecipeDescriptionController
       ["edit",s] -> controllerOnKey s editRecipeDescriptionController
       ["delete",s] -> controllerOnKey s deleteRecipeDescriptionController
       ["destroy",s] -> controllerOnKey s destroyRecipeDescriptionController
       _ -> displayUrlError

------------------------------------------------------------------------------
--- The type of a new RecipeDescription entity.
type NewRecipeDescription = (String,String,String,String,String,Recipe)

--- Shows a form to create a new RecipeDescription entity.
newRecipeDescriptionController :: Controller
newRecipeDescriptionController =
  checkAuthorization (categoryOperationAllowed NewEntity) $ \_ -> do
    allRecipes <- runQ queryAllRecipes
    setParWuiStore wuiNewRecipeDescriptionStore allRecipes
                   ("", "", "", "", "", head allRecipes)
    return [formExp newRecipeDescriptionForm]

--- Supplies a WUI form to create a new RecipeDescription entity.
--- The fields of the entity have some default values.
newRecipeDescriptionForm ::
  HtmlFormDef ([Recipe], WuiStore NewRecipeDescription)
newRecipeDescriptionForm =
  pwui2FormDef "Controller.RecipeDescription.newRecipeDescriptionForm"
    wuiNewRecipeDescriptionStore
    (\possibleRecipes -> wRecipeDescription possibleRecipes)
    (\_ entity -> transactionController
                    (runT (createRecipeDescriptionT entity))
                    (nextInProcessOr (redirectController "?RecipeDescription/list")
                      Nothing))
    (renderWUI "Create new RecipeDescription" "Create"
        "?RecipeDescription/list")

---- The data stored for executing the WUI form.
wuiNewRecipeDescriptionStore ::
  Global (SessionStore ([Recipe], WuiStore NewRecipeDescription))
wuiNewRecipeDescriptionStore =
  global emptySessionStore
         (Persistent (inDataDir "wuiNewRecipeDescriptionStore"))

--- Transaction to persist a new RecipeDescription entity to the database.
createRecipeDescriptionT :: NewRecipeDescription -> DBAction ()
createRecipeDescriptionT
    (servings,ingredients,directions,prepTime,cookTime,recipe) =
  newRecipeDescriptionWithRecipeRecDescKey servings ingredients directions
   prepTime
   cookTime
   (recipeKey recipe)
   >+= (\_ -> return ())

------------------------------------------------------------------------------
--- Shows a form to edit the given RecipeDescription entity.
editRecipeDescriptionController :: RecipeDescription -> Controller
editRecipeDescriptionController recdescr =
  checkAuthorization (recipeDescriptionOperationAllowed (UpdateEntity recdescr))
   $ \_ -> do
      allRecipes <- runQ queryAllRecipes
      recDescRecipe <- runJustT (getRecDescRecipe recdescr)
      setParWuiStore wuiEditRecipeDescriptionStore
                     (recdescr, recDescRecipe, allRecipes)
                     recdescr
      return [formExp editRecipeDescriptionForm]

--- Supplies a WUI form to edit a given RecipeDescription entity.
--- The fields of the entity have some default values.
editRecipeDescriptionForm ::
  HtmlFormDef ((RecipeDescription, Recipe, [Recipe]),
               WuiStore RecipeDescription)
editRecipeDescriptionForm =
  pwui2FormDef "Controller.RecipeDescription.editRecipeDescriptionForm"
    wuiEditRecipeDescriptionStore
    (\ (recipeDescription, relatedRecipe, possibleRecipes) ->
       wRecipeDescriptionType recipeDescription relatedRecipe possibleRecipes)
    (\_ entity -> transactionController (runT (updateRecipeDescriptionT entity))
                    (nextInProcessOr (redirectController "?RecipeDescription/list")
                      Nothing))
    (renderWUI "Edit RecipeDescription" "Change"
                  "?RecipeDescription/list")

---- The data stored for executing the WUI form.
wuiEditRecipeDescriptionStore ::
   Global (SessionStore ((RecipeDescription, Recipe, [Recipe]),
                         WuiStore RecipeDescription))
wuiEditRecipeDescriptionStore =
  global emptySessionStore
         (Persistent (inDataDir "wuiEditRecipeDescriptionStore"))

--- Transaction to persist modifications of a given RecipeDescription entity
--- to the database.
updateRecipeDescriptionT :: RecipeDescription -> DBAction ()
updateRecipeDescriptionT recipeDescription =
  updateRecipeDescription recipeDescription

------------------------------------------------------------------------------
--- Deletes a given RecipeDescription entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteRecipeDescriptionController :: RecipeDescription -> Controller
deleteRecipeDescriptionController recipeDescription =
  checkAuthorization
   (recipeDescriptionOperationAllowed (DeleteEntity recipeDescription)) $ \_ ->
     confirmDeletionPage
       (concat ["Really delete entity \""
               ,recipeDescriptionToShortView recipeDescription
                ,"\"?"])

--- Deletes a given RecipeDescription entity
--- and proceeds with the list controller.
destroyRecipeDescriptionController :: RecipeDescription -> Controller
destroyRecipeDescriptionController recipeDescription =
  checkAuthorization
   (recipeDescriptionOperationAllowed (DeleteEntity recipeDescription))
   $ (\_ ->
     transactionController (runT (deleteRecipeDescriptionT recipeDescription))
      (redirectController "?RecipeDescription/list"))

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
