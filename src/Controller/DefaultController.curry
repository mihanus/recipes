module Controller.DefaultController where

import Controller.Category
import System.SessionInfo
import System.Spicey

--- The default controller of the application.
defaultController :: Controller
defaultController = do
  catkeys <- getCurrentCats
  listCategoryControllerWithArgs ("list" : catkeys)
