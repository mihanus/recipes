----------------------------------------------------------------------------
--- This module defines the data that is associated to a user session
--- and passed to a view so that views can be adapted to user sessions.
---
--- Currently, the session data contains information about the login
--- status of a user. Typically, this information will be extended
--- according to the concrete application.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module System.SessionInfo
  ( UserSessionInfo(..), userLoginOfSession, setUserLoginOfSession
  , getUserSessionInfo, updateUserSessionInfo
  , getCurrentCats, storeCurrentCats
  ) where

import Global

import HTML.Session

import Config.Storage ( inDataDir )

--------------------------------------------------------------------------
--- The data associated to a user session.
--- It contains the list of category keys of the current navigation
--- and information about the login status of a user.
--- The argument of the session data is `Nothing` if the user is not logged in.
--- Otherwise, it is `Maybe ln` where `ln` is the login name of the user.
data UserSessionInfo = SD [String] (Maybe String)

--- The initial (empty) session data
emptySessionInfo :: UserSessionInfo
emptySessionInfo = SD [] Nothing

--- Extracts the login status from the user session data.
userLoginOfSession :: UserSessionInfo -> Maybe String
userLoginOfSession (SD _ login) = login

--- Sets the login status of the user session data.
setUserLoginOfSession :: Maybe String -> UserSessionInfo -> UserSessionInfo
setUserLoginOfSession login (SD cks _) = SD cks login

--- Extracts the login status from the user session data.
userCatsOfSession :: UserSessionInfo -> [String]
userCatsOfSession (SD cks _) = cks

--- Sets the current category keys of the user session data.
setUserCats :: [String] -> UserSessionInfo -> UserSessionInfo
setUserCats catkeys (SD _ login) = SD catkeys login

--------------------------------------------------------------------------
--- Definition of the session state to store the login name (as a string).
userSessionInfo :: Global (SessionStore UserSessionInfo)
userSessionInfo =
  global emptySessionStore (Persistent (inDataDir "userSessionInfo"))

--- Gets the data of the current user session.
getUserSessionInfo :: IO UserSessionInfo
getUserSessionInfo =
  getSessionData userSessionInfo emptySessionInfo

--- Updates the data of the current user session.
updateUserSessionInfo :: (UserSessionInfo -> UserSessionInfo) -> IO ()
updateUserSessionInfo = updateSessionData userSessionInfo emptySessionInfo

--------------------------------------------------------------------------

--- Gets the current category navigation.
getCurrentCats :: IO [String]
getCurrentCats = getUserSessionInfo >>= return . userCatsOfSession

--- Stores the current category navigation.
storeCurrentCats :: [String] -> IO ()
storeCurrentCats catkeys = updateUserSessionInfo (setUserCats catkeys)

--------------------------------------------------------------------------
