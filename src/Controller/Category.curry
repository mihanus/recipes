module Controller.Category
  ( mainCategoryController, newCategoryForm, editCategoryForm
  , listCategoryController, listCategoryControllerWithArgs )
 where

import Global
import List(last)
import Time

import HTML.Base
import HTML.Session
import HTML.WUI

import Config.Storage
import System.Spicey
import Recipes
import View.Category
import Maybe
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import View.EntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a Category entity according to the URL parameter.
mainCategoryController :: Controller
mainCategoryController =
  do args <- getControllerParams
     case args of
       [] -> listCategoryController
       ("list":_) -> listCategoryController
       ["new",s] -> controllerOnKey s newCategoryController
       ["show",s] -> controllerOnKey s showCategoryController
       ["edit",s] -> controllerOnKey s editCategoryController
       ["delete",s] -> controllerOnKey s deleteCategoryController
       ["destroy",s] -> controllerOnKey s destroyCategoryController
       _ -> displayUrlError

------------------------------------------------------------------------------
--- The type of a new Category entity.
type NewCategory = (String,Int)

--- Shows a form to create a new Category entity inside the given category.
newCategoryController :: Category -> Controller
newCategoryController cat =
  checkAuthorization (categoryOperationAllowed NewEntity) $ \_ -> do
    listurl <- getCurrentCatsURL
    setParWuiStore newCategoryStore (cat,listurl) ("",0)
    return [formExp newCategoryForm]

--- Supplies a WUI form to create a new Category entity.
--- The fields of the entity have some default values.
newCategoryForm :: HtmlFormDef ((Category,String), WuiStore NewCategory)
newCategoryForm =
  pwui2FormDef "Controller.Category.newCategoryForm"
    newCategoryStore
    (\_ -> wCategory)
    (\ (cat,listurl) entity -> do
       transactionController (runT (createCategoryT cat entity))
         (nextInProcessOr (redirectController listurl) Nothing))
    (\ (_,listurl) -> renderWUI "Neue Kategorie" "Speichern" listurl ())

---- The data stored for executing the WUI form.
newCategoryStore ::
  Global (SessionStore ((Category,String), WuiStore NewCategory))
newCategoryStore =
  global emptySessionStore (Persistent (inDataDir "newCategoryStore"))

--- Transaction to persist a new Category entity to the database.
createCategoryT :: Category -> (String,Int) -> DBAction ()
createCategoryT cat (name,position) =
  newCategoryWithCategoryParentCategoryKey name position
   (Just (categoryKey cat))
   >+= (\_ -> return ())

------------------------------------------------------------------------------
--- Shows a form to edit the given Category entity.
editCategoryController :: Category -> Controller
editCategoryController categoryToEdit =
  checkAuthorization (categoryOperationAllowed (UpdateEntity categoryToEdit))
   $ \_ -> do
      setParWuiStore wuiEditCategoryStore categoryToEdit categoryToEdit
      return [formExp editCategoryForm]

--- Supplies a WUI form to edit a given Category entity.
--- The fields of the entity have some default values.
editCategoryForm :: HtmlFormDef (Category, WuiStore Category)
editCategoryForm =
  pwui2FormDef "Controller.Category.editCategoryForm"
    wuiEditCategoryStore
    (\cat -> wCategoryType cat)
    (\cat entity -> do
       listurl <- getCurrentCatsURL
       checkAuthorization (categoryOperationAllowed (UpdateEntity cat)) $ \_ ->
         transactionController (runT (updateCategory entity))
           (nextInProcessOr (redirectController listurl) Nothing))
    (renderWUI "Kategorie ändern" "Speichern" "?Category/list")

---- The data stored for executing the WUI form.
wuiEditCategoryStore :: Global (SessionStore (Category, WuiStore Category))
wuiEditCategoryStore =
  global emptySessionStore (Persistent (inDataDir "wuiEditCategoryStore"))

------------------------------------------------------------------------------
--- Deletes a given Category entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteCategoryController :: Category -> Controller
deleteCategoryController category =
  checkAuthorization (categoryOperationAllowed (DeleteEntity category)) $ \_ ->
    confirmDeletionPage
      (concat ["Really delete entity \"",categoryToShortView category,"\"?"])

--- Deletes a given Category entity
--- and proceeds with the list controller.
destroyCategoryController :: Category -> Controller
destroyCategoryController category = do
  listurl <- getCurrentCatsURL
  checkAuthorization (categoryOperationAllowed (DeleteEntity category))
   $ (\_ ->
     transactionController (runT (deleteCategoryT category))
      (redirectController listurl))

--- Transaction to delete a given Category entity.
deleteCategoryT :: Category -> DBAction ()
deleteCategoryT category =
  (getCategoryRecipes category
    >+= (\oldRecipeCategoryRecipes ->
      removeRecipeCategory oldRecipeCategoryRecipes category))
   >+ deleteCategory category

--- Lists Category entities in a given category navigation.
--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listCategoryController :: Controller
listCategoryController = do
  givenargs <- getControllerParams
  listCategoryControllerWithArgs givenargs

--- Lists all Category entities with buttons to show, delete,
--- or edit an entity.
listCategoryControllerWithArgs :: [String] -> Controller
listCategoryControllerWithArgs givenargs =
  checkAuthorization (categoryOperationAllowed ListEntities) $ \sinfo -> do
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
    storeCurrentCats args
    return$ listCategoryView sinfo
              (zip parentcatnames args) currentcat
              categorys recipes
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
