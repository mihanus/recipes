module Controller.Category
  ( mainCategoryController, listCategoryController )
 where

import List(last)
import System.Spicey
import HTML.Base
import Time
import Recipes
import View.Category
import Maybe
import Controller.Recipe
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import View.RecipesEntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a Category entity according to the URL parameter.
mainCategoryController :: Controller
mainCategoryController =
  do args <- getControllerParams
     case args of
       [] -> listCategoryController
       ("list":_) -> listCategoryController
       ["new",s] ->
         applyControllerOn (readCategoryKey s) (runJustT . getCategory)
          newCategoryController
       ["show",s] ->
         applyControllerOn (readCategoryKey s) (runJustT . getCategory)
          showCategoryController
       ["edit",s] ->
         applyControllerOn (readCategoryKey s) (runJustT . getCategory)
          editCategoryController
       ["delete",s] ->
         applyControllerOn (readCategoryKey s) (runJustT . getCategory)
          deleteCategoryController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new Category entity.
newCategoryController :: Category -> Controller
newCategoryController cat =
  checkAuthorization (categoryOperationAllowed NewEntity)
   $ (\sinfo ->
     do return
         (blankCategoryView sinfo
           (\entity ->
             transactionController (runT (createCategoryT cat entity))
              (nextInProcessOr listCategoryController Nothing))
           listCategoryController))

--- Transaction to persist a new Category entity to the database.
createCategoryT :: Category -> (String,Int) -> DBAction ()
createCategoryT cat (name,position) =
  newCategoryWithCategoryParentCategoryKey name position
   (Just (categoryKey cat))
   >+= (\_ -> return ())

--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $ (\sinfo ->
     do return
         (editCategoryView sinfo categoryToEdit
           (\entity ->
             transactionController (runT (updateCategory entity))
              (nextInProcessOr listCategoryController Nothing))
           listCategoryController))

--- Deletes a given Category entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteCategoryController :: Category -> Controller
deleteCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
            ["Really delete entity \"",categoryToShortView category,"\"?"])]]
      (transactionController (runT (deleteCategoryT category))
        listCategoryController)
      (showCategoryController category))

--- Transaction to delete a given Category entity.
deleteCategoryT :: Category -> DBAction ()
deleteCategoryT category =
  (getCategoryRecipes category
    >+= (\oldRecipeCategoryRecipes ->
      removeRecipeCategory oldRecipeCategoryRecipes category))
   >+ deleteCategory category

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listCategoryController :: Controller
listCategoryController =
  checkAuthorization (categoryOperationAllowed ListEntities)
   $ (\sinfo ->
    do givenargs <- getControllerParams
       args <- if length givenargs <= 1
               then do rc <- runQ queryRootCategory
                       return ([showCategoryKey rc])
               else return (tail givenargs) -- first arg = "list"
       ckey <- maybe (runQ queryRootCategory >>= return . categoryKey)
                     return
                     (readCategoryKey (last args))
       currentcat <- runJustT (getCategory ckey)
       categorys <- runQ $ queryCategoryContent ckey
       recipes  <- runJustT $ getRecipesInCategory ckey
       parentcatnames <- runJustT $ mapM stringcatkey2catname args
       saveCurrentCategory (categoryName currentcat)
       return
        (listCategoryView sinfo
          (zip parentcatnames args) (categoryName currentcat)
          categorys recipes showCategoryController
          editCategoryController deleteCategoryController
          (newCategoryController currentcat)
          (newRecipeDescController currentcat)
          (newRecipeController currentcat)
          (addRecipeController currentcat)))
 where
  stringcatkey2catname s =
    maybe (return "")
          (\ckey -> getCategory ckey >>= return . categoryName)
          (readCategoryKey s)

--- Shows a Category entity.
showCategoryController :: Category -> Controller
showCategoryController category =
  checkAuthorization (categoryOperationAllowed (ShowEntity category))
   $ (\sinfo ->
     do recipeCategoryRecipes <- runJustT (getCategoryRecipes category)
        return (showCategoryView sinfo category recipeCategoryRecipes))

--- Associates given entities with the Category entity.
addRecipeCategory :: [Recipe] -> Category -> DBAction ()
addRecipeCategory recipes category =
  mapM_ (\t -> newRecipeCategory (categoryKey category) (recipeKey t)) recipes

--- Removes association to the given entities with the Category entity.
removeRecipeCategory :: [Recipe] -> Category -> DBAction ()
removeRecipeCategory recipes category =
  mapM_ (\t -> deleteRecipeCategory (categoryKey category) (recipeKey t))
   recipes

--- query the root category
queryRootCategory :: DBAction Category
queryRootCategory =
  liftM head
    (queryCondCategory (\c -> categoryCategoryParentCategoryKey c == Nothing))

--- query contents of a category with a given key
queryCategoryContent :: CategoryID -> DBAction [Category]
queryCategoryContent pkey =
  queryCondCategory (\c -> categoryCategoryParentCategoryKey c == Just pkey)

--- query recipes of a category with a given key (Nothing: root categories)
getRecipesInCategory :: CategoryID -> DBAction [Recipe]
getRecipesInCategory pkey = getCategory pkey >>= getCategoryRecipes
