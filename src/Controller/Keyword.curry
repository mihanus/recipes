module Controller.Keyword
  ( mainKeywordController, newKeywordForm
  ) where

import Data.List
import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI

import Config.EntityRoutes
import Config.Storage
import Model.Recipes
import Model.SQL_Queries
import View.Keyword
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import Config.UserProcesses
import View.Recipe
import View.EntitiesToHtml
import Database.CDBI.Connection

--- Choose the controller for a Keyword entity according to the URL parameter.
mainKeywordController :: Controller
mainKeywordController =
  do args <- getControllerParams
     case args of
       [] -> listKeywordController
       ["list"]   -> listKeywordController
       ["all"]    -> listAllKeywordController
       ["char",c] -> listCharKeywordController (head (urlencoded2string c))
       ["new"]  -> newKeywordController
       ["show",s] -> controllerOnKey s showKeywordController
       ["edit",s] -> controllerOnKey s editKeywordController
       ["delete",s] -> controllerOnKey s deleteKeywordController
       ["destroy",s] -> controllerOnKey s destroyKeywordController
       _ -> displayError "Illegal URL"

------------------------------------------------------------------------------
--- The type of a new Keyword entity.
type NewKeyword = String

--- Shows a form to create a new Keyword entity.
newKeywordController :: Controller
newKeywordController =
  checkAuthorization (keywordOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore newKeywordStore sinfo ""
        return [formElem newKeywordForm])

--- A WUI form to create a new Keyword entity.
--- The default values for the fields are stored in 'newKeywordStore'.
newKeywordForm :: HtmlFormDef (UserSessionInfo,WuiStore NewKeyword)
newKeywordForm =
  pwui2FormDef "Controller.Keyword.newKeywordForm"
    newKeywordStore
    (\_ -> wKeyword)
    (\_ entity -> transactionController (runT (createKeywordT entity))
                    (nextInProcessOr (redirectController "?Keyword/list") Nothing))
    (\sinfo ->
      renderWUI sinfo "Neues Stichwort" "Speichern" "?Keyword/list" ())

--- The data stored for executing the "new entity" WUI form.
newKeywordStore :: SessionStore (UserSessionInfo,WuiStore NewKeyword)
newKeywordStore = sessionStore "newKeywordStore"

--- Transaction to persist a new Keyword entity to the database.
createKeywordT :: String -> DBAction ()
createKeywordT name = newKeyword name >>= (\_ -> return ())

------------------------------------------------------------------------------
--- Shows a form to edit the given Keyword entity.
editKeywordController :: Keyword -> Controller
editKeywordController keywordToEdit =
  checkAuthorization (keywordOperationAllowed (UpdateEntity keywordToEdit))
   $ \sinfo -> do
      setParWuiStore editKeywordStore (sinfo,keywordToEdit) keywordToEdit
      return [formElem editKeywordForm]

--- A WUI form to edit a Keyword entity.
--- The default values for the fields are stored in 'editKeywordStore'.
editKeywordForm :: HtmlFormDef ((UserSessionInfo,Keyword),WuiStore Keyword)
editKeywordForm =
  pwui2FormDef "Controller.Keyword.editKeywordForm"
    editKeywordStore
    (\(_,keyword) -> wKeywordType keyword)
    (\_ entity -> transactionController (runT (updateKeywordT entity))
                    (nextInProcessOr (redirectController "?Keyword/list") Nothing))
    (\(sinfo,_) -> renderWUI sinfo "Stichwort ändern" "Speichern" "?Keyword/list" ())

--- The data stored for executing the edit WUI form.
editKeywordStore :: SessionStore ((UserSessionInfo,Keyword),WuiStore Keyword)
editKeywordStore = sessionStore "editKeywordStore"

--- Transaction to persist modifications of a given Keyword entity
--- to the database.
updateKeywordT :: Keyword -> DBAction ()
updateKeywordT keyword = updateKeyword keyword

------------------------------------------------------------------------------
--- Deletes a given Keyword entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteKeywordController :: Keyword -> Controller
deleteKeywordController keyword =
  checkAuthorization (keywordOperationAllowed (DeleteEntity keyword)) $ \sinfo ->
     confirmDeletionPage sinfo
       (concat
          ["Stichwort \"",keywordToShortView keyword,"\" wirklich löschen?"])

--- Deletes a given Keyword entity
--- and proceeds with the list controller.
destroyKeywordController :: Keyword -> Controller
destroyKeywordController keyword =
  checkAuthorization (keywordOperationAllowed (DeleteEntity keyword))
   $ (\_ ->
     transactionController (runT (deleteKeywordT keyword))
      (redirectController "?Keyword/list"))

--- Transaction to delete a given Keyword entity.
deleteKeywordT :: Keyword -> DBAction ()
deleteKeywordT keyword = deleteKeyword keyword

--- Lists Keyword entities.
listKeywordController :: Controller
listKeywordController =
  checkAuthorization (keywordOperationAllowed ListEntities) $ \_ -> do
    kws <- runQ queryAllKeywords
    return (keywordAlphabetView
              (map head
                   (group
                      (map (\s -> if null s then ' ' else head s)
                           (sort (map keywordName kws))))))

--- Lists all keywords.
listAllKeywordController :: Controller
listAllKeywordController =
  checkAuthorization (keywordOperationAllowed ListEntities) $ \sinfo ->
    runQ queryAllKeywords >>= return . listKeywordView sinfo "Alle Stichworte"

--- Lists all keywords starting with a given character.
listCharKeywordController :: Char -> Controller
listCharKeywordController c =
  checkAuthorization (keywordOperationAllowed ListEntities) $ \sinfo ->
    runQ (queryCondKeyword (\k -> let kn = keywordName k
                                  in not (null kn) && head kn == c))
      >>= return . listKeywordView sinfo ("Stichworte: "++[c]++"...")

--- Shows a Keyword entity.
showKeywordController :: Keyword -> Controller
showKeywordController keyword =
  checkAuthorization (keywordOperationAllowed (ShowEntity keyword))
   $ \sinfo -> do
     recipes <- runJustT $ getKeywordRecipes keyword
     return (listRecipesOfKeyword sinfo keyword recipes)
