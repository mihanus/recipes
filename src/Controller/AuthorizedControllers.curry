module System.AuthorizedActions where

import System.Authorization
import System.SessionInfo
import Recipes

-- Only listing is allow for non-admins:
anyOperationAllowed :: AccessType _ -> UserSessionInfo -> IO AccessResult
anyOperationAllowed at sinfo = do
  return $ if at==ListEntities || isAdminSession sinfo
             then AccessGranted
             else AccessDenied "Operation not allowed"

--- Checks whether the application of an operation to a Category
--- entity is allowed.
categoryOperationAllowed
  :: AccessType Category -> UserSessionInfo -> IO AccessResult
categoryOperationAllowed = anyOperationAllowed

--- Checks whether the application of an operation to a Keyword
--- entity is allowed.
keywordOperationAllowed
  :: AccessType Keyword -> UserSessionInfo -> IO AccessResult
keywordOperationAllowed = anyOperationAllowed

--- Checks whether the application of an operation to a Recipe
--- entity is allowed.
recipeOperationAllowed
  :: AccessType Recipe -> UserSessionInfo -> IO AccessResult
recipeOperationAllowed = anyOperationAllowed

--- Checks whether the application of an operation to a RecipeDescription
--- entity is allowed.
recipeDescriptionOperationAllowed
  :: AccessType RecipeDescription -> UserSessionInfo -> IO AccessResult
recipeDescriptionOperationAllowed = anyOperationAllowed
