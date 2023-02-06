module Controller.Recipe
  ( mainRecipeController
  , createRecipeForm, createRecipeDescForm, editRecipeForm, addRecipeForm
  ) where

import System.PreludeHelpers

import Data.List        ( delete, intersect, intersperse, sortBy )
import Data.Maybe
import Data.Time
import System.Directory ( doesFileExist, renameFile )
import System.FilePath  ( (</>) )
import System.Process   ( system )

import Config.EntityRoutes
import Config.Storage
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
import System.Helpers
import Config.UserProcesses
import View.EntitiesToHtml
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
    ("show":s:ckeys) -> controllerOnKey s (showRecipeController ckeys)
    ("edit":s:ckeys) -> controllerOnKey s (editRecipeController ckeys)
    ["delete",s] -> controllerOnKey s (deleteRecipeController Nothing)
    ["delete",s,ckey] ->
       controllerOnKey s (deleteRecipeController (Just ckey))
    ["destroy",s] -> controllerOnKey s (destroyRecipeController Nothing)
    ["destroy",s,ckey] ->
       controllerOnKey s (destroyRecipeController (Just ckey))
    ["uploadpic",s]   -> controllerOnKey s uploadPictureRecipeController
    ["uploadedpic",s] -> controllerOnKey s checkUploadPictureRecipeController
    ["uploadpdf",s]   -> controllerOnKey s uploadPDFRecipeController
    ["uploadedpdf",s] -> controllerOnKey s checkUploadPDFRecipeController
    _ -> displayUrlError

--------------------------------------------------------------------------
--- Shows a form to edit the given Category entity.
addRecipeController :: String -> Controller
addRecipeController catkey =
  checkAuthorization (recipeOperationAllowed NewEntity) $ \sinfo -> do
    cat <- runJustT $ stringcatkey2cat catkey
    allRecipes <- runQ queryAllRecipes >>= return . sortBy leqRecipe
    listurl <- getCurrentCatsURL
    setParWuiStore addRecipeStore (sinfo, cat, allRecipes, listurl)
                   (head allRecipes)
    return [formElem addRecipeForm]

--- Supplies a WUI form to edit a given Category entity.
--- The fields of the entity have some default values.
addRecipeForm ::
  HtmlFormDef ((UserSessionInfo,Category,[Recipe],String), WuiStore Recipe)
addRecipeForm =
  pwui2FormDef "Controller.Recipe.addRecipeForm"
    addRecipeStore
    (\ (_,_,allrecipes,_) -> wSelect recipeToShortView allrecipes)
    (\ (_,cat,_,listurl) entity ->
       checkAuthorization (recipeOperationAllowed NewEntity) $ \_ ->
         transactionController (runT (addGivenRecipeT cat entity))
           (nextInProcessOr (redirectController listurl) Nothing))
    (\ (sinfo,_,_,listurl) ->
       renderWUI sinfo "Rezept in Kategorie hinzufügen" "Hinzufügen" listurl ())
 where
  addGivenRecipeT cat rec = newRecipeCategory (categoryKey cat) (recipeKey rec)

---- The data stored for executing the WUI form.
addRecipeStore ::
  SessionStore ((UserSessionInfo,Category,[Recipe],String), WuiStore Recipe)
addRecipeStore = sessionStore "addRecipeStore"

------------------------------------------------------------------------------
--- Shows a form to create a new Recipe entity in a category.
newRecipeController :: String -> Controller
newRecipeController catkey =
  checkAuthorization (recipeOperationAllowed NewEntity)
   $ \sinfo -> do
    cat <- runJustT $ stringcatkey2cat catkey
    listurl <- getCurrentCatsURL
    setParWuiStore createRecipeStore (sinfo,cat,listurl) ("","","")
    return [formElem createRecipeForm]

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
createRecipeForm ::
  HtmlFormDef ((UserSessionInfo,Category,String), WuiStore NewRecipe)
createRecipeForm = pwui2FormDef "Controller.Recipe.createRecipeForm"
  createRecipeStore
    (\_ -> wRecipe)
    (\ (_,cat,listurl) entity ->
       checkAuthorization (recipeOperationAllowed NewEntity) $ \_ ->
          transactionController (runT (createRecipeT cat entity))
             (nextInProcessOr (redirectController listurl) Nothing))
    (\ (sinfo,_,listurl) ->
        renderWUI sinfo "Neue Rezeptreferenz" "Speichern" listurl ())

---- The data stored for executing the WUI form.
createRecipeStore :: SessionStore ((UserSessionInfo,Category,String), WuiStore NewRecipe)
createRecipeStore = sessionStore "createRecipeStore"

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
    listurl <- getCurrentCatsURL
    setParWuiStore createRecipeDescStore
                   (sinfo,cat,listurl) ("","","","","","","","")
    return [formElem createRecipeDescForm]

type NewRecipeDesc = (String,String,String,String,String,String,String,String)

--- The form definition to create a new Recipe entity
--- containing the controller to insert a new Recipe entity.
createRecipeDescForm ::
  HtmlFormDef ((UserSessionInfo,Category,String), WuiStore NewRecipeDesc)
createRecipeDescForm = pwui2FormDef "Controller.Recipe.createRecipeDescForm"
  createRecipeDescStore
  (\_ -> wRecipeDesc)
  (\ (_,cat,listurl) entity ->
     checkAuthorization (recipeOperationAllowed NewEntity) $ \_ ->
       transactionController (runT (createRecipeDescT cat entity))
         (nextInProcessOr (redirectController listurl) Nothing))
    (\ (sinfo,_,listurl) -> renderWUI sinfo "Neues Rezept" "Speichern" listurl ())

---- The data stored for executing the WUI form.
createRecipeDescStore ::
  SessionStore ((UserSessionInfo,Category,String), WuiStore NewRecipeDesc)
createRecipeDescStore = sessionStore "createRecipeDescStore"

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
    listurl <- getCurrentCatsURL
    setParWuiStore editRecipeStore
      (sinfo, recipe, mbrecdesc, parentcatkeys, listurl)
      (recipe, keywords2string recipekeywords, mbrecdesc)
    return [formElem editRecipeForm]

--- The form definition to edit an existing Person entity
--- containing the controller to update the entity.
editRecipeForm ::
  HtmlFormDef ((UserSessionInfo,Recipe,Maybe RecipeDescription,[String],String),
               WuiStore (Recipe,String,Maybe RecipeDescription))
editRecipeForm = pwui2FormDef "Controller.Recipe.editRecipeForm"
    editRecipeStore
    (\ (_,recipe,mbrecdesc,_,_) -> wRecipeType recipe mbrecdesc)
    (\ (_,_,_,parentcatkeys,_) entity@(recipe,_,_) ->
     checkAuthorization (recipeOperationAllowed (UpdateEntity recipe)) $ \_ ->
     transactionController (runT (updateRecipeT entity))
       (nextInProcessOr
         (runJustT (getRecipe (recipeKey recipe)) >>=
          showRecipeController parentcatkeys) Nothing))
    (\ (sinfo,_,_,_,listurl) -> renderWUI sinfo "Rezept ändern" "Speichern" listurl ())

--- The data stored for executing the WUI form.
editRecipeStore ::
  SessionStore ((UserSessionInfo,
                 Recipe,Maybe RecipeDescription,[String],String),
                WuiStore (Recipe,String,Maybe RecipeDescription))
editRecipeStore = sessionStore "editRecipeStore"

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
  checkAuthorization (recipeOperationAllowed (DeleteEntity recipe)) $ \sinfo ->
    confirmDeletionPage sinfo
      (concat ["Rezept \"", recipeToShortView recipe, "\" ",
               maybe "wirklich" (\_ -> "in der Kategorie") mbcatkey,
               " löschen?"])

--- Transaction to delete a given Recipe entity.
--- If a category is given, it is only deleted from this category,
--- otherwise it will be complete deleted.
destroyRecipeController :: Maybe String -> Recipe -> Controller
destroyRecipeController mbcatkey recipe = do
  listurl <- getCurrentCatsURL
  checkAuthorization (recipeOperationAllowed (DeleteEntity recipe)) $ \_ -> do
    transactionController (runT (deleteRecipeT mbcatkey recipe))
                          (redirectController listurl)

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

------------------------------------------------------------------------------
--- A controller to upload a picture for a given Recipe entity.
uploadPictureRecipeController :: Recipe -> Controller
uploadPictureRecipeController recipe =
  checkAuthorization (recipeOperationAllowed (UpdateEntity recipe)) $ \_ ->
  return
    [h3 [htxt "Bild für dieses Rezept hochladen"],
     uploadForm ".jpg,.JPG,.JPEG,.jpeg" (rkey ++ ".jpg")
                ("spicey.cgi?Recipe/uploadedpic/" ++ rkey)]
 where
  rkey = showRecipeKey recipe

--- A controller to check and process and uploaded picture file.
checkUploadPictureRecipeController :: Recipe -> Controller
checkUploadPictureRecipeController recipe =
  checkAuthorization (recipeOperationAllowed (UpdateEntity recipe)) $ \_ -> do
    expic <- doesFileExist uplpicfile
    isjpg <- isJpegFile uplpicfile
    if expic && isjpg
      then do
        system $ "chmod 644 " ++ uplpicfile
        renameFile uplpicfile (pictureDir </> picfile)
        setPageMessage $ "Neues Bild zum Rezept '" ++ recipeName recipe ++
                         "' hinzugefügt"
      else setPageMessage $ if expic then "Nichts hochgeaden: kein JPEG!"
                                     else "Nichts hochgeladen"
    getCurrentCatsURL >>= redirectController
 where
  picfile    = showRecipeKey recipe ++ ".jpg"
  uplpicfile = "uploads" </> picfile

--------------------------------------------------------------------------
--- A controller to upload a PDF for a given Recipe entity.
uploadPDFRecipeController :: Recipe -> Controller
uploadPDFRecipeController recipe =
  checkAuthorization (recipeOperationAllowed (UpdateEntity recipe)) $ \_ ->
  return
    [h3 [htxt "PDF für dieses Rezept hochladen"],
     uploadForm ".pdf" (rkey ++ ".pdf")
                ("spicey.cgi?Recipe/uploadedpdf/" ++ rkey)]
 where
  rkey = showRecipeKey recipe

--- A controller to check and process and uploaded picture file.
checkUploadPDFRecipeController :: Recipe -> Controller
checkUploadPDFRecipeController recipe =
  checkAuthorization (recipeOperationAllowed (UpdateEntity recipe)) $ \_ -> do
    expdf <- doesFileExist uplpdffile
    ispdf <- isPdfFile uplpdffile
    if expdf && ispdf
      then do
        system $ "chmod 644 " ++ uplpdffile
        renameFile uplpdffile (pdfDir </> pdffile)
        setPageMessage $ "Neues PDF zum Rezept '" ++ recipeName recipe ++
                         "' hinzugefügt"
      else setPageMessage $ if expdf then "Nichts hochgeladen: kein PDF"
                                     else "Nichts hochgeladen"
    getCurrentCatsURL >>= redirectController
 where
  pdffile    = showRecipeKey recipe ++ ".pdf"
  uplpdffile = "uploads" </> pdffile

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
  checkAuthorization (recipeOperationAllowed (ShowEntity recipe)) $ \sinfo -> do
    keywords <- runJustT (getRecipeKeywords recipe)
    recdesc <- runQ $ queryDescriptionOfRecipe (recipeKey recipe)
    parentcats <- runJustT $ mapM stringcatkey2cat ckeys
    let picfile = pictureDir </> showRecipeKey recipe ++ ".jpg"
    expic <- doesFileExist picfile
    let pdffile = pdfDir </> showRecipeKey recipe ++ ".pdf"
    expdf <- doesFileExist pdffile
    return $ singleRecipeView sinfo (zip parentcats ckeys) recipe keywords
               recdesc
               (if expic then Just picfile else Nothing)
               (if expdf then Just pdffile else Nothing)

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
