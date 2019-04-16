module System.AuthorizedActions where

import System.Authorization
import System.SessionInfo
import Recipes

--- Checks whether the application of an operation to a Category
--- entity is allowed.
categoryOperationAllowed
  :: AccessType Category -> UserSessionInfo -> IO AccessResult
categoryOperationAllowed at _ =
  case at of
    ListEntities -> return AccessGranted
    NewEntity -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    DeleteEntity _ -> return AccessGranted
    UpdateEntity _ -> return AccessGranted

--- Checks whether the application of an operation to a Keyword
--- entity is allowed.
keywordOperationAllowed
  :: AccessType Keyword -> UserSessionInfo -> IO AccessResult
keywordOperationAllowed at _ =
  case at of
    ListEntities -> return AccessGranted
    NewEntity -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    DeleteEntity _ -> return AccessGranted
    UpdateEntity _ -> return AccessGranted

--- Checks whether the application of an operation to a Recipe
--- entity is allowed.
recipeOperationAllowed
  :: AccessType Recipe -> UserSessionInfo -> IO AccessResult
recipeOperationAllowed at _ =
  case at of
    ListEntities -> return AccessGranted
    NewEntity -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    DeleteEntity _ -> return AccessGranted
    UpdateEntity _ -> return AccessGranted

--- Checks whether the application of an operation to a RecipeDescription
--- entity is allowed.
recipeDescriptionOperationAllowed
  :: AccessType RecipeDescription -> UserSessionInfo -> IO AccessResult
recipeDescriptionOperationAllowed at _ =
  case at of
    ListEntities -> return AccessGranted
    NewEntity -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    DeleteEntity _ -> return AccessGranted
    UpdateEntity _ -> return AccessGranted