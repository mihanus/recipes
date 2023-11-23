module Config.EntityRoutes () where

import System.Spicey
import Model.Recipes

instance EntityController Category where
  controllerOnKey s =
    applyControllerOn (readCategoryKey s) (runJustT . getCategory)

  entityRoute r ent = concat ["?Category/",r,"/",showCategoryKey ent]

instance EntityController Keyword where
  controllerOnKey s =
    applyControllerOn (readKeywordKey s) (runJustT . getKeyword)

  entityRoute r ent = concat ["?Keyword/",r,"/",showKeywordKey ent]

instance EntityController Recipe where
  controllerOnKey s =
    applyControllerOn (readRecipeKey s) (runJustT . getRecipe)

  entityRoute r ent = concat ["?Recipe/",r,"/",showRecipeKey ent]

instance EntityController RecipeDescription where
  controllerOnKey s =
    applyControllerOn (readRecipeDescriptionKey s)
     (runJustT . getRecipeDescription)

  entityRoute r ent =
    concat ["?RecipeDescription/",r,"/",showRecipeDescriptionKey ent]