module Controller.Keyword
   ( mainKeywordController, newKeywordForm
   ) where

import Global
import List
import Maybe
import Sort
import Time

import HTML.Base
import HTML.Session
import HTML.WUI

import Config.Storage
import Recipes
import SQL_Queries
import View.Keyword
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import Config.UserProcesses
import View.Recipe
import View.RecipesEntitiesToHtml
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
       ["show",s] -> applyKeywordControllerOn s showKeywordController
       ["edit",s] -> applyKeywordControllerOn s editKeywordController
       ["delete",s] -> applyKeywordControllerOn s deleteKeywordController
       ["destroy",s] -> applyKeywordControllerOn s destroyKeywordController
       _ -> displayError "Illegal URL"

--- Applies a keyword controller on the keyword specified by the
--- key (first argument).
applyKeywordControllerOn
  :: String -> (Keyword -> Controller) -> Controller
applyKeywordControllerOn s =
  applyControllerOn (readKeywordKey s) (runJustT . getKeyword)

------------------------------------------------------------------------------
--- The type of a new Keyword entity.
type NewKeyword = String

--- Shows a form to create a new Keyword entity.
newKeywordController :: Controller
newKeywordController =
  checkAuthorization (keywordOperationAllowed NewEntity) $ \_ -> do
    setParWuiStore wuiNewKeywordStore () ""
    return [formExp newKeywordForm]

--- Supplies a WUI form to create a new Keyword entity.
--- The fields of the entity have some default values.
newKeywordForm :: HtmlFormDef ((), WuiStore NewKeyword)
newKeywordForm =
  pwui2FormDef "Controller.Keyword.newKeywordForm"
    wuiNewKeywordStore
    (\_ -> wKeyword)
    (\_ entity -> transactionController (runT (createKeywordT entity))
                    (nextInProcessOr listKeywordController Nothing))
    (renderWuiExp "Neues Stichwort" "Speichern" listKeywordController)

---- The data stored for executing the WUI form.
wuiNewKeywordStore :: Global (SessionStore ((), WuiStore NewKeyword))
wuiNewKeywordStore =
  global emptySessionStore (Persistent (inDataDir "wuiNewKeywordStore"))

--- Transaction to persist a new Keyword entity to the database.
createKeywordT :: String -> DBAction ()
createKeywordT name = newKeyword name >+= (\_ -> return ())

------------------------------------------------------------------------------
--- Shows a form to edit the given Keyword entity.
editKeywordController :: Keyword -> Controller
editKeywordController keywordToEdit =
  checkAuthorization (keywordOperationAllowed (UpdateEntity keywordToEdit))
   $ \_ -> do
      setParWuiStore wuiEditKeywordStore keywordToEdit keywordToEdit
      return [formExp editKeywordForm]

--- Supplies a WUI form to edit a given Keyword entity.
--- The fields of the entity have some default values.
editKeywordForm :: HtmlFormDef (Keyword, WuiStore Keyword)
editKeywordForm =
  pwui2FormDef "Controller.Keyword.editKeywordForm"
    wuiEditKeywordStore
    (\keyword -> wKeywordType keyword)
    (\_ entity -> transactionController (runT (updateKeywordT entity))
                    (nextInProcessOr listKeywordController Nothing))
    (renderWuiExp "Stichwort ändern" "Speichern" listKeywordController)

---- The data stored for executing the WUI form.
wuiEditKeywordStore :: Global (SessionStore (Keyword, WuiStore Keyword))
wuiEditKeywordStore =
  global emptySessionStore (Persistent (inDataDir "wuiEditKeywordStore"))

--- Transaction to persist modifications of a given Keyword entity
--- to the database.
updateKeywordT :: Keyword -> DBAction ()
updateKeywordT keyword = updateKeyword keyword

------------------------------------------------------------------------------
--- Deletes a given Keyword entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteKeywordController :: Keyword -> Controller
deleteKeywordController keyword =
  checkAuthorization (keywordOperationAllowed (DeleteEntity keyword)) $ \_ ->
     confirmDeletionPage
       (concat
          ["Stichwort \"",keywordToShortView keyword,"\" wirklich löschen?"])

--- Deletes a given Keyword entity.
destroyKeywordController :: Keyword -> Controller
destroyKeywordController keyword =
  checkAuthorization (keywordOperationAllowed (DeleteEntity keyword)) $ \_ ->
    transactionController (runT (deleteKeywordT keyword))
                          listKeywordController

--- Transaction to delete a given Keyword entity.
deleteKeywordT :: Keyword -> DBAction ()
deleteKeywordT keyword = deleteKeyword keyword

--- Lists Keyword entities.
listKeywordController :: Controller
listKeywordController =
  checkAuthorization (keywordOperationAllowed ListEntities) $ \_ -> do
    saveCurrentCategory ""
    kws <- runQ queryAllKeywords
    return (keywordAlphabetView
              (map head
                   (group
                      (map (\s -> if null s then ' ' else head s)
                           (Sort.sort (map keywordName kws))))))

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
