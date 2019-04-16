module Controller.DefaultController where

import Controller.Category
import System.Spicey

--- The default controller of the application.
defaultController :: Controller
defaultController = listCategoryController
