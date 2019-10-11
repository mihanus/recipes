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
import HTML.Session
import System.Processes
import System.Authentication
import View.SpiceySystem
import Controller.DefaultController

-----------------------------------------------------------------------------
--- Controller for login/logout.
loginController :: Controller
loginController = do
  login <- getSessionLogin
  putSessionData loginFormStore login
  return [formExp loginFormDef]

loginFormDef :: HtmlFormDef (Maybe String)
loginFormDef =
  HtmlFormDef "Controller.SpiceySystem.loginFormDef"
    (getSessionData loginFormStore Nothing)
    (loginView defaultController)

--- The data processed by the login form.
loginFormStore :: Global (SessionStore (Maybe String))
loginFormStore =
  global emptySessionStore (Persistent (inDataDir "loginFormStore"))

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
