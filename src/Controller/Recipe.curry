module Controller.Recipe
  ( mainRecipeController
  , createRecipeForm, createRecipeDescForm, editRecipeForm, addRecipeForm
  ) where

import Global
import List  ( delete, intersect, intersperse )
import Maybe
import Sort  ( sortBy )
import Time

import Config.Storage
import Controller.DefaultController
import HTML.Base
import HTML.Session
import HTML.WUI
import Recipes
import SQL_Queries
import View.Recipe
import System.Spicey
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import Config.UserProcesses
import View.RecipesEntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a Recipe entity according to the URL parameter.
mainRecipeController :: Controller
mainRecipeController = do
  args <- getControllerParams
  case args of
    [] -> listRecipeController
    ["list"] -> listRecipeController
    ["add",catkey] -> addRecipeController catkey
    ["new",catkey] -> newRecipeDescController catkey
    ["newref",catkey] -> newRecipeController catkey
    ("show":s:ckeys) -> applyRecipeControllerOn s (showRecipeController ckeys)
    ("edit":s:ckeys) -> applyRecipeControllerOn s (editRecipeController ckeys)
    ["delete",s] -> applyRecipeControllerOn s (deleteRecipeController Nothing)
    ["delete",s,ckey] ->
       applyRecipeControllerOn s (deleteRecipeController (Just ckey))
    ["destroy",s] -> applyRecipeControllerOn s (destroyRecipeController Nothing)
    ["destroy",s,ckey] ->
       applyRecipeControllerOn s (destroyRecipeController (Just ckey))
    _ -> displayUrlError

applyRecipeControllerOn :: String -> (Recipe -> Controller) -> Controller
applyRecipeControllerOn s =
  applyControllerOn (readRecipeKey s) (runJustT . getRecipe)

--------------------------------------------------------------------------
--- Shows a form to edit the given Category entity.
addRecipeController :: String -> Controller
addRecipeController catkey =
  checkAuthorization (recipeOperationAllowed NewEntity) $ \_ -> do
    cat <- runJustT $ stringcatkey2cat catkey
    allRecipes <- runQ queryAllRecipes >>= return . sortBy leqRecipe
    setParWuiStore wuiAddRecipeStore (cat, allRecipes) (head allRecipes)
    return [formExp addRecipeForm]

--- Supplies a WUI form to edit a given Category entity.
--- The fields of the entity have some default values.
addRecipeForm :: HtmlFormDef ((Category,[Recipe]), WuiStore Recipe)
addRecipeForm =
  pwui2FormDef "Controller.Recipe.addRecipeForm"
    wuiAddRecipeStore
    (\ (_,allrecipes) -> wSelect recipeToShortView allrecipes)
    (\ (cat,_) entity ->
       checkAuthorization (recipeOperationAllowed NewEntity) $ \_ ->
         transactionController (runT (addGivenRecipeT cat entity))
           (nextInProcessOr defaultController Nothing))
    (renderWuiExp "Rezept in Kategorie hinzufügen" "Hinzufügen"
                  defaultController)
 where
  addGivenRecipeT cat rec = newRecipeCategory (categoryKey cat) (recipeKey rec)

---- The data stored for executing the WUI form.
wuiAddRecipeStore ::
  Global (SessionStore ((Category,[Recipe]), WuiStore Recipe))
wuiAddRecipeStore =
  global emptySessionStore (Persistent (inDataDir "wuiAddRecipeStore"))

------------------------------------------------------------------------------
--- Shows a form to create a new Recipe entity in a category.
newRecipeController :: String -> Controller
newRecipeController catkey =
  checkAuthorization (recipeOperationAllowed NewEntity)
   $ \sinfo -> do
    cat <- runJustT $ stringcatkey2cat catkey
    setParWuiStore wuiCreateRecipeStore (sinfo,cat) ("","","")
    return [par [htxt explainPDF], formExp createRecipeForm]

explainPDF :: String
explainPDF = unlines
  [ "Falls ein PDF für dieses Rezept vorhanden ist, dann kann man dieses "
  , "unter '~/public_html/SAM/recipes_archive' abspeichern und in der "
  , "Rezeptreferenz die Zeichenfolge '<dateiname.pdf>' einfügen. "
  , "Diese wird dann als Link auf das PDF angezeigt."
  ]

type NewRecipe = (String,String,String)

--- The form definition to create a new Recipe entity
--- containing the controller to insert a new Recipe entity.
createRecipeForm :: HtmlFormDef ((UserSessionInfo,Category), WuiStore NewRecipe)
createRecipeForm = pwui2FormDef "Controller.Recipe.createRecipeForm"
  wuiCreateRecipeStore
    (\_ -> wRecipe)
    (\ (_,cat) entity ->
       checkAuthorization (recipeOperationAllowed NewEntity) $ \_ ->
          transactionController (runT (createRecipeT cat entity))
             (nextInProcessOr defaultController Nothing))
    (renderWuiExp "Neue Rezeptreferenz" "Speichern" defaultController)

---- The data stored for executing the WUI form.
wuiCreateRecipeStore ::
  Global (SessionStore ((UserSessionInfo,Category), WuiStore NewRecipe))
wuiCreateRecipeStore =
  global emptySessionStore (Persistent (inDataDir "wuiCreateRecipeStore"))

--- Transaction to persist a new Recipe entity to the database.
createRecipeT :: Category -> NewRecipe -> DBAction ()
createRecipeT cat (name,reference,keywords) = do
  rec <- newRecipe name reference
  newRecipeCategory (categoryKey cat) (recipeKey rec)
  _ <- addKeywords (string2keywords keywords) rec
  return ()

------------------------------------------------------------------------------
--- Shows a form to create a new Recipe entity in a category.
newRecipeDescController :: String -> Controller
newRecipeDescController catkey =
  checkAuthorization (recipeOperationAllowed NewEntity)
   $ \sinfo -> do
    cat <- runJustT $ stringcatkey2cat catkey
    setParWuiStore wuiCreateRecipeDescStore
                   (sinfo,cat) ("","","","","","","","")
    return [par [htxt explainPDF], formExp createRecipeDescForm]

type NewRecipeDesc = (String,String,String,String,String,String,String,String)

--- The form definition to create a new Recipe entity
--- containing the controller to insert a new Recipe entity.
createRecipeDescForm ::
  HtmlFormDef ((UserSessionInfo,Category), WuiStore NewRecipeDesc)
createRecipeDescForm = pwui2FormDef "Controller.Recipe.createRecipeDescForm"
  wuiCreateRecipeDescStore
  (\_ -> wRecipeDesc)
  (\ (_,cat) entity ->
     checkAuthorization (recipeOperationAllowed NewEntity) $ \_ ->
       transactionController (runT (createRecipeDescT cat entity))
         (nextInProcessOr defaultController Nothing))
    (renderWuiExp "Neues Rezept" "Speichern" defaultController)

---- The data stored for executing the WUI form.
wuiCreateRecipeDescStore ::
  Global (SessionStore ((UserSessionInfo,Category), WuiStore NewRecipeDesc))
wuiCreateRecipeDescStore =
  global emptySessionStore (Persistent (inDataDir "wuiCreateRecipeDescStore"))

--- Transaction to persist a new Recipe entity to the database.
createRecipeDescT :: Category -> NewRecipeDesc -> DBAction ()
createRecipeDescT cat
  (name,reference,keywords,servings,ingredients,directions,prepTime,cookTime) =
  do rec <- newRecipe name reference
     newRecipeCategory (categoryKey cat) (recipeKey rec)
     newRecipeDescriptionWithRecipeRecDescKey servings
            ingredients directions prepTime cookTime (recipeKey rec)
     _ <- addKeywords (string2keywords keywords) rec
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

--------------------------------------------------------------------------
--- Shows a form to edit the given Recipe entity.
editRecipeController :: [String] -> Recipe -> Controller
editRecipeController parentcatkeys recipe =
  checkAuthorization (recipeOperationAllowed (UpdateEntity recipe))
   $ \sinfo -> do
    mbrecdesc <- runQ $ queryDescriptionOfRecipe (recipeKey recipe)
    recipekeywords <- runJustT (getRecipeKeywords recipe)
    setParWuiStore wuiEditRecipeStore
      (sinfo, recipe, mbrecdesc, parentcatkeys)
      (recipe, keywords2string recipekeywords, mbrecdesc)
    return [formExp editRecipeForm]

--- The form definition to edit an existing Person entity
--- containing the controller to update the entity.
editRecipeForm ::
  HtmlFormDef ((UserSessionInfo,Recipe,Maybe RecipeDescription,[String]),
               WuiStore (Recipe,String,Maybe RecipeDescription))
editRecipeForm = pwui2FormDef "Controller.Recipe.editRecipeForm"
    wuiEditRecipeStore
    (\ (_,recipe,mbrecdesc,_) -> wRecipeType recipe mbrecdesc)
    (\ (_,_,_,parentcatkeys) entity@(recipe,_,_) ->
     checkAuthorization (recipeOperationAllowed (UpdateEntity recipe)) $ \_ ->
     transactionController (runT (updateRecipeT entity))
       (nextInProcessOr
         (runJustT (getRecipe (recipeKey recipe)) >>=
          showRecipeController parentcatkeys) Nothing))
    (renderWuiExp "Rezept ändern" "Speichern" defaultController)

--- The data stored for executing the WUI form.
wuiEditRecipeStore ::
  Global (SessionStore ((UserSessionInfo,
                         Recipe,Maybe RecipeDescription,[String]),
                        WuiStore (Recipe,String,Maybe RecipeDescription)))
wuiEditRecipeStore =
  global emptySessionStore (Persistent (inDataDir "wuiEditRecipeStore"))

--- Transaction to persist modifications of a given Recipe entity
--- to the database.
updateRecipeT :: (Recipe,String,Maybe RecipeDescription) -> DBAction ()
updateRecipeT (recipe,keywords,mbrecdesc) = do
  updateRecipe recipe
  maybe (return ()) updateRecipeDescription mbrecdesc
  oldTaggingKeywords <- getRecipeKeywords recipe
  _ <- updateKeywords oldTaggingKeywords (string2keywords keywords) recipe
  return ()

updateKeywords :: [Keyword] -> [String] -> Recipe -> DBAction [Keyword]
updateKeywords oldkeywords newkeywords recipe = do
  removeTagging delkeywords recipe
  addKeywords addkeywords recipe
 where
  unchanged   = intersect (map keywordName oldkeywords) newkeywords
  delkeywords = filter (\okw -> keywordName okw `notElem` unchanged) oldkeywords
  addkeywords = foldr delete newkeywords unchanged

--------------------------------------------------------------------------
--- Deletes a given Recipe entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteRecipeController :: Maybe String -> Recipe -> Controller
deleteRecipeController mbcatkey recipe =
  checkAuthorization (recipeOperationAllowed (DeleteEntity recipe)) $ \_ ->
    confirmDeletionPage
      (concat ["Rezept \"", recipeToShortView recipe, "\" ",
               maybe "wirklich" (\_ -> "in der Kategorie") mbcatkey,
               " löschen?"])

--- Transaction to delete a given Recipe entity.
--- If a category is given, it is only deleted from this category,
--- otherwise it will be complete deleted.
destroyRecipeController :: Maybe String -> Recipe -> Controller
destroyRecipeController mbcatkey recipe =
  checkAuthorization (recipeOperationAllowed (DeleteEntity recipe)) $ \_ -> do
    transactionController (runT (deleteRecipeT mbcatkey recipe))
                          defaultController

--- Transaction to delete a given Recipe entity.
--- If a category is given, it is only deleted from this category,
--- otherwise it will be complete deleted.
deleteRecipeT :: Maybe String -> Recipe -> DBAction ()
deleteRecipeT Nothing recipe = do
  oldTaggingKeywords <- getRecipeKeywords recipe
  removeTagging oldTaggingKeywords recipe
  mbrecdesc <- queryDescriptionOfRecipe (recipeKey recipe)
  maybe (return ()) deleteRecipeDescription mbrecdesc
  deleteRecipe recipe
deleteRecipeT (Just catkey) recipe = do
  cat <- stringcatkey2cat catkey
  deleteRecipeCategory (categoryKey cat) (recipeKey recipe)

--------------------------------------------------------------------------
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
                                recipe keywords recdesc)

--- Gets the category corresponding to a given category key string.
stringcatkey2cat :: String -> DBAction Category
stringcatkey2cat s = maybe (fail "Illegal key") getCategory (readCategoryKey s)

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
