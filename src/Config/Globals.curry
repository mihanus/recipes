-- Some global settings for the recipe database
module Config.Globals where

--- Location of the directory containing private run-time data
--- such as session and authentication information.
spiceyDataDir :: String
spiceyDataDir = "data"

-- The directory containing all recipe data:
recipeDataDir = "/net/medoc/home/mh/home/data/recipes"

recipeDB = recipeDataDir++"/Recipes.db"

-- Standard login name
defaultLoginName = "gourmet"

-- File containing hash code of default login
defaultHashFile = recipeDataDir++"/.rezlogin"
