module Controller.Keyword ( mainKeywordController ) where

import HTML.Base
import List
import Maybe
import Sort
import Time
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
       ["show",s] ->
         applyControllerOn (readKeywordKey s) (runJustT . getKeyword)
          showKeywordController
       ["edit",s] ->
         applyControllerOn (readKeywordKey s) (runJustT . getKeyword)
          editKeywordController
       ["delete",s] ->
         applyControllerOn (readKeywordKey s) (runJustT . getKeyword)
          deleteKeywordController
       _ -> displayError "Illegal URL"

--- Shows a form to create a new Keyword entity.
newKeywordController :: Controller
newKeywordController =
  checkAuthorization (keywordOperationAllowed NewEntity)
   $ (\sinfo ->
     do return
         (blankKeywordView sinfo
           (\entity ->
             transactionController (runT (createKeywordT entity))
              (nextInProcessOr listKeywordController Nothing))
           listKeywordController))

--- Transaction to persist a new Keyword entity to the database.
createKeywordT :: String -> DBAction ()
createKeywordT name = newKeyword name >+= (\_ -> return ())

--- Shows a form to edit the given Keyword entity.
editKeywordController :: Keyword -> Controller
editKeywordController keywordToEdit =
  checkAuthorization (keywordOperationAllowed (UpdateEntity keywordToEdit))
   $ (\sinfo ->
     do return
         (editKeywordView sinfo keywordToEdit
           (\entity ->
             transactionController (runT (updateKeywordT entity))
              (nextInProcessOr listKeywordController Nothing))
           listKeywordController))

--- Transaction to persist modifications of a given Keyword entity
--- to the database.
updateKeywordT :: Keyword -> DBAction ()
updateKeywordT keyword = updateKeyword keyword

--- Deletes a given Keyword entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteKeywordController :: Keyword -> Controller
deleteKeywordController keyword =
  checkAuthorization (keywordOperationAllowed (DeleteEntity keyword))
   $ (\_ ->
     confirmController
      [h3
        [htxt
          (concat
           ["Stichwort \"",keywordToShortView keyword,"\" wirklich löschen?"])]]
      (transactionController (runT (deleteKeywordT keyword))
        listKeywordController)
      (showKeywordController keyword))

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
