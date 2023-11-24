-- Some global settings for the recipe database
module Config.Globals where

import System.FilePath ( (</>) )

--- Location of the directory containing private run-time data
--- such as session and authentication information.
spiceyDataDir :: String
spiceyDataDir = "data"

-- The directory containing all recipe data:
recipeDataDir :: String
--recipeDataDir = "/net/medoc/home/mh/home/data/recipes"
recipeDataDir = "../recipeData"

recipeDB :: String
recipeDB = recipeDataDir </> "Recipes.db"

-- File containing the standard login name
defaultLoginFile :: String
defaultLoginFile = recipeDataDir </> ".rezlogin"

-- File containing hash code of default login
defaultHashFile :: String
defaultHashFile = recipeDataDir </> ".rezhash"
