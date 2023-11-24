--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.SpiceySystem
  ( loginView, processListView, historyView )
 where

import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefScndSmButton, primSmButton, scndButton )

import Config.Globals
import Config.UserProcesses
import System.Processes
import System.Spicey
import System.Authentication

-----------------------------------------------------------------------------
--- View for login/logout. If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
--- If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: (Maybe String, String) -> [HtmlExp]
loginView (currlogin, listurl) =
  case currlogin of
   Nothing -> [h3 [htxt "Login as:"],
               htxt "Login name:", nbsp, 
               textField loginfield "" `addAttr` ("autofocus",""), nbsp,
               htxt "Password:", nbsp, password passwdfield, nbsp,
               primSmButton "Anmelden" loginHandler]
   Just _  -> [h3 [htxt "Wirklich abmelden?"],
               primSmButton "Abmelden" logoutHandler,
               hrefScndSmButton listurl [htxt "Abbrechen"]]
 where
  loginfield, passwdfield free
  
  loginHandler env = do
    let loginname = env loginfield
        passwd    = env passwdfield
    if null passwd
      then return ()
      else do adminlogin <- readFile defaultLoginFile
              hash       <- getUserHash loginname passwd
              storedhash <- readFile defaultHashFile
              if loginname == adminlogin && hash == storedhash
                then do loginToSession loginname
                        setPageMessage $ "Angemeldet als: " ++ loginname
                else setPageMessage "Login failed: wrong password"
    nextInProcessOr (redirectController listurl) Nothing >>= getPage
  
  logoutHandler _ = do
    logoutFromSession >> setPageMessage "Abgemeldet"
    nextInProcessOr (redirectController listurl) Nothing >>= getPage

-----------------------------------------------------------------------------
--- A view for all processes contained in a given process specification.
processListView :: Processes a -> [BaseHtml]
processListView procs =
  [h1 [htxt "Processes"],
   ulist (map processColumn (zip (processNames procs) [1..]))]
 where
   processColumn (pname, id) =
     [href ("?spiceyProcesses/"++show id) [htxt pname]]

-----------------------------------------------------------------------------
--- A view for all URLs of a session.
historyView :: [String] -> [BaseHtml]
historyView urls =
  [h1 [htxt "History"],
   ulist (map (\url -> [href ("?"++url) [htxt url]])
              (filter (not . null) urls))]

-----------------------------------------------------------------------------
