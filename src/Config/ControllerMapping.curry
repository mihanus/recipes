module Config.ControllerMapping where

import System.Spicey
import System.Routes
import Controller.SpiceySystem
import Config.RoutesData
import Controller.Category
import Controller.Keyword
import Controller.Recipe
import Controller.RecipeDescription
import Controller.Search

--- Maps the controllers associated to URLs in module RoutesData
--- into the actual controller operations.
getController :: ControllerReference -> Controller
getController fktref =
  case fktref of
    ProcessListController -> processListController
    SearchController -> searchController
    LoginController -> loginController
    CategoryController -> mainCategoryController
    KeywordController -> mainKeywordController
    RecipeController -> mainRecipeController
    RecipeDescriptionController -> mainRecipeDescriptionController
    _ -> displayError "getController: no mapping found"