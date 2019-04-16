module Controller.Recipe
  ( mainRecipeController, addRecipeController
  , newRecipeController, newRecipeDescController
  ) where

import List ( delete, intersect, intersperse )
import System.Spicey
import HTML.Base
import Time
import Recipes
import SQL_Queries
import View.Recipe
import Maybe
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import View.Recipe
import View.RecipesEntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a Recipe entity according to the URL parameter.
mainRecipeController :: Controller
mainRecipeController =
  do args <- getControllerParams
     case args of
       [] -> listRecipeController
       ["list"] -> listRecipeController
       --["new"] -> newRecipeController
       ("show":s:ckeys) ->
         applyControllerOn (readRecipeKey s) (runJustT . getRecipe)
          (showRecipeController ckeys)
       --["edit",s] ->
       --  applyControllerOn (readRecipeKey s) (runJustT . getRecipe)
       --   editRecipeController
       ["delete",s] ->
         applyControllerOn (readRecipeKey s) (runJustT . getRecipe)
          (deleteRecipeController Nothing)
       _ -> displayError "Illegal URL"

--- Adds an existing Recipe entity to a given category.
addRecipeController :: Category -> Controller
addRecipeController cat =
  checkAuthorization (recipeOperationAllowed NewEntity) $ \sinfo -> do
    allRecipes <- runQ queryAllRecipes
    return (addRecipeView sinfo allRecipes
             (\entity ->
               transactionController (runT (addGivenRecipeT cat entity))
                (nextInProcessOr listRecipeController Nothing))
             listRecipeController)
 where
  addGivenRecipeT :: Category -> Recipe -> DBAction ()
  addGivenRecipeT cat rec = newRecipeCategory (categoryKey cat) (recipeKey rec)

--- Shows a form to create a new Recipe entity in a category.
newRecipeController :: Category -> Controller
newRecipeController cat =
  checkAuthorization (recipeOperationAllowed NewEntity)
   $ (\sinfo ->
     do return
         (blankRecipeView sinfo
           (\entity ->
             transactionController (runT (createRecipeT cat entity))
              (nextInProcessOr listRecipeController Nothing))
           listRecipeController))

--- Transaction to persist a new Recipe entity to the database.
createRecipeT :: Category -> (String,String,String) -> DBAction ()
createRecipeT cat (name,reference,keywords) = do
  rec <- newRecipe name reference
  newRecipeCategory (categoryKey cat) (recipeKey rec)
  kws <- addKeywords (string2keywords keywords) rec
  return ()

--- Shows a form to create a new Recipe entity in a category.
newRecipeDescController :: Category -> Controller
newRecipeDescController cat =
  checkAuthorization (recipeOperationAllowed NewEntity)
   $ (\sinfo ->
     do return
         (blankRecipeDescView sinfo
           (\entity ->
             transactionController (runT (createRecipeDescT cat entity))
              (nextInProcessOr listRecipeController Nothing))
           listRecipeController))

--- Transaction to persist a new Recipe entity to the database.
createRecipeDescT :: Category
  -> (String,String,String,String,String,String,String,String) -> DBAction ()
createRecipeDescT cat
  (name,reference,keywords,servings,ingredients,directions,prepTime,cookTime) =
  do rec <- newRecipe name reference
     newRecipeCategory (categoryKey cat) (recipeKey rec)
     newRecipeDescriptionWithRecipeRecDescKey servings
            ingredients directions prepTime cookTime (recipeKey rec)
     kws <- addKeywords (string2keywords keywords) rec
     return ()

setStoreMessage :: String -> [Keyword] -> IO ()
setStoreMessage name newkws = setPageMessage $
  name ++ " " ++
  if null newkws
   then "gespeichert"
   else ("und neue Stichworte " ++ showKeywordNames newkws ++ " gespeichert")
 where
  showKeywordNames :: [Keyword] -> String
  showKeywordNames = concat . intersperse " " . map keywordName

--- Shows a form to edit the given Recipe entity.
editRecipeController :: [String] -> Recipe -> Maybe RecipeDescription
                     -> Controller
editRecipeController parentcatkeys recipe mbrecdesc =
  checkAuthorization (recipeOperationAllowed (UpdateEntity recipe))
   $ \sinfo -> do
       recipekeywords <- runJustT (getRecipeKeywords recipe)
       return
         (editRecipeView sinfo
           (recipe, keywords2string recipekeywords, mbrecdesc)
           (\entity ->
             transactionController (runT (updateRecipeT entity))
              (nextInProcessOr
                 (runJustT (getRecipe (recipeKey recipe)) >>= showRecipeController parentcatkeys) Nothing))
           (showRecipeController parentcatkeys recipe))

--- Transaction to persist modifications of a given Recipe entity
--- to the database.
updateRecipeT :: (Recipe,String,Maybe RecipeDescription) -> DBAction ()
updateRecipeT (recipe,keywords,mbrecdesc) = do
  updateRecipe recipe
  maybe (return ()) updateRecipeDescription mbrecdesc
  oldTaggingKeywords <- getRecipeKeywords recipe
  kws <- updateKeywords oldTaggingKeywords (string2keywords keywords) recipe
  return ()

updateKeywords :: [Keyword] -> [String] -> Recipe -> DBAction [Keyword]
updateKeywords oldkeywords newkeywords recipe = do
  removeTagging delkeywords recipe
  addKeywords addkeywords recipe
 where
  unchanged   = intersect (map keywordName oldkeywords) newkeywords
  delkeywords = filter (\okw -> keywordName okw `notElem` unchanged) oldkeywords
  addkeywords = foldr delete newkeywords unchanged

--- Deletes a given Recipe entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteRecipeController :: Maybe Category -> Recipe -> Controller
deleteRecipeController mbcat recipe =
  checkAuthorization (recipeOperationAllowed (DeleteEntity recipe))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Rezept \"",recipeToShortView recipe,"\" wirklich löschen?"])]]
      (transactionController (runT (deleteRecipeT mbcat recipe))
        listRecipeController)
      (showRecipeController [] recipe))

--- Transaction to delete a given Recipe entity.
--- If a category is given, it is only deleted from this category,
--- otherwise it will be complete deleted.
deleteRecipeT :: Maybe Category -> Recipe -> DBAction ()
deleteRecipeT Nothing recipe = do
  oldTaggingKeywords <- getRecipeKeywords recipe
  removeTagging oldTaggingKeywords recipe
  mbrecdesc <- queryDescriptionOfRecipe (recipeKey recipe)
  maybe (return ()) deleteRecipeDescription mbrecdesc
  deleteRecipe recipe

deleteRecipeT (Just cat) recipe =
  deleteRecipeCategory (categoryKey cat) (recipeKey recipe)

--- Lists all Recipe entities with buttons to show, delete,
--- or edit an entity.
listRecipeController :: Controller
listRecipeController =
  checkAuthorization (recipeOperationAllowed ListEntities)
   $ (\sinfo ->
     do recipes <- runQ queryAllRecipes
        return (listRecipeView sinfo "Alle Rezepte" recipes))

--- Shows a Recipe entity.
showRecipeController :: [String] -> Recipe -> Controller
showRecipeController ckeys recipe =
  checkAuthorization (recipeOperationAllowed (ShowEntity recipe))
   $ \sinfo ->
    do keywords <- runJustT (getRecipeKeywords recipe)
       recdesc <- runQ $ queryDescriptionOfRecipe (recipeKey recipe)
       parentcats <- runJustT $ mapM stringcatkey2cat ckeys
       return (singleRecipeView sinfo (zip parentcats ckeys)
                 recipe keywords recdesc
                 editRecipeController deleteRecipeController)
 where
  stringcatkey2cat s =
    maybe (fail "Illegal key")
          getCategory
          (readCategoryKey s)

--- Associates given entities with the Recipe entity.
addTagging :: [Keyword] -> Recipe -> DBAction ()
addTagging keywords recipe =
  mapM_ (\t -> newTagging (recipeKey recipe) (keywordKey t)) keywords

--- Associates a list of keywords to a given Recipe entity.
--- Keyword entities will be created if the keyword does not exist.
--- The list of newly created entities will be returned
addKeywords :: [String] -> Recipe -> DBAction [Keyword]
addKeywords keywords recipe = mapM addKeyword keywords >>= return . catMaybes
 where
  addKeyword :: String -> DBAction (Maybe Keyword)
  addKeyword s = do
    kws <- queryAKeyword s
    new <- if null kws then newKeyword s >>= return . Just
                       else return Nothing
    newTagging (recipeKey recipe) (keywordKey (maybe (head kws) id new))
    return new

--- Removes association to the given entities with the Recipe entity.
removeTagging :: [Keyword] -> Recipe -> DBAction ()
removeTagging keywords recipe =
  mapM_ (\t -> deleteTagging (recipeKey recipe) (keywordKey t)) keywords