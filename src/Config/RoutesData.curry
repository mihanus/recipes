module Config.RoutesData where

import System.Authentication

data ControllerReference = ProcessListController
                         | SearchController
                         | LoginController
                         | CategoryController
                         | KeywordController
                         | RecipeController
                         | RecipeDescriptionController

data UrlMatch = Exact String
              | Prefix String String
              | Matcher (String -> Bool)
              | Always

type Route = (String,UrlMatch,ControllerReference)

--- This constant specifies the association of URLs to controllers.
--- Controllers are identified here by constants of type
--- ControllerReference. The actual mapping of these constants
--- into the controller operations is specified in the module
--- ControllerMapping.
getRoutes :: IO [Route]
getRoutes =
  do login <- getSessionLogin
     return
      [--("Processes",Exact "spiceyProcesses",ProcessListController)
       ("Kategorien",Prefix "Category" "list",CategoryController)
      --,("New Category",Prefix "Category" "new",CategoryController)
      ,("Alle Stichworte",Prefix "Keyword" "list",KeywordController)
      --,("New Keyword",Prefix "Keyword" "new",KeywordController)
      ,("Alle Rezepte (alphabetisch)",Prefix "Recipe" "list",RecipeController)
      ,("Alle Rezepte (neue zuerst)",Prefix "Recipe" "listid",RecipeController)
      --,("New Recipe",Prefix "Recipe" "new",RecipeController)
      --,("List RecipeDescription"
      -- ,Prefix "RecipeDescription" "list"
      -- ,RecipeDescriptionController)
      --,("New RecipeDescription"
      -- ,Prefix "RecipeDescription" "new"
      -- ,RecipeDescriptionController)
      ,("Suche",Prefix "Search" "main",SearchController)
      ,(maybe "Anmelden" (const "Abmelden") login,Exact "login",LoginController)
      ,("default",Always,CategoryController)]