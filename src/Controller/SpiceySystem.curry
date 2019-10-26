--------------------------------------------------------------------------
--- This module contains some controller that might be used in in
--- Spicey application.
--- In particular, it provides a controller for login/out and
--- a controller to start selected user processes.
--------------------------------------------------------------------------

module Controller.SpiceySystem
  ( loginController, loginFormDef
  , processListController, historyController
  )
 where

import Global
import ReadNumeric

import Config.Storage
import Config.UserProcesses
import System.Spicey
import HTML.Base
import HTML.Session
import System.Processes
import System.Authentication
import View.SpiceySystem

-----------------------------------------------------------------------------
--- Controller for login/logout.
loginController :: Controller
loginController = do
  login <- getSessionLogin
  listurl   <- getCurrentCatsURL
  putSessionData loginViewData (login,listurl)
  return [formExp loginFormDef]

loginFormDef :: HtmlFormDef (Maybe String, String)
loginFormDef = formDefWithID "Controller.SpiceySystem.loginFormDef"
  (getSessionData loginViewData (Nothing, "")) loginView

--- The data processed by the login form.
loginViewData :: Global (SessionStore (Maybe String, String))
loginViewData =
  global emptySessionStore (Persistent (inDataDir "loginViewData"))

-----------------------------------------------------------------------------
--- Controller for showing and selecting user processes.
processListController :: Controller
processListController = do
  args <- getControllerParams
  if null args
   then return $ processListView availableProcesses
   else case (readInt (head args)) of
          Just (idInt, _) -> do
            startProcess (processNames availableProcesses !! (idInt - 1))
          Nothing ->
            displayError "could not read process id"

-----------------------------------------------------------------------------
--- Controller for the URL history.
historyController :: Controller
historyController = getLastUrls >>= return . historyView

-----------------------------------------------------------------------------
