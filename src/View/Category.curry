module View.Category
  ( wCategory, tuple2Category, category2Tuple, wCategoryType
  , showCategoryView, listCategoryView
  ) where

import Data.List
import Data.Time
import HTML.Base
import HTML.WUI
import HTML.Styles.Bootstrap4
import Config.EntityRoutes
import System.Authentication
import System.Spicey
import System.SessionInfo
import Recipes
import View.EntitiesToHtml
import View.Recipe ( leqRecipe )

--- The WUI specification for the entity type Category.
--- It also includes fields for associated entities.
wCategory :: WuiSpec (String,Int)
wCategory =
  withRendering
   (wPair wRequiredString wInt)
   (renderLabels (take 2 categoryLabelList))

--- Transformation from data of a WUI form to entity type Category.
tuple2Category :: Category -> (String,Int) -> Category
tuple2Category categoryToUpdate (name ,position) =
  (setCategoryName
    (setCategoryPosition categoryToUpdate position)
    name)

--- Transformation from entity type Category to a tuple
--- which can be used in WUI specifications.
category2Tuple :: Category -> (String,Int)
category2Tuple category =
  (categoryName category,categoryPosition category)

--- WUI Type for editing or creating Category entities.
--- Includes fields for associated entities.
wCategoryType :: Category -> WuiSpec Category
wCategoryType category =
  transformWSpec (tuple2Category category,category2Tuple)
                 wCategory

------------------------------------------------------------------------------
--- Supplies a view to show the details of a Category.
showCategoryView :: UserSessionInfo -> Category -> [Recipe] -> [BaseHtml]
showCategoryView _ category recipes =
  categoryToDetailsView category recipes
   ++ [hrefPrimSmButton "?Category/list" [htxt "back to Category list"]]

--- Compares two Category entities. This order is used in the list view.
leqCategory :: Category -> Category -> Bool
leqCategory x1 x2 = categoryPosition x1 <= categoryPosition x2

--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the user session info, the name and key strings
--- of all categories from the root to the current category,
--- the current category, and the list of categories and recipes
--- contained in this category.
listCategoryView
 :: UserSessionInfo -> [(String,String)] -> Category -> [Category] -> [Recipe]
  -> [BaseHtml]
listCategoryView sinfo parentcats currentcat categorys recipes =
  [h4 (concatMap
         (\ (n,(cname,a)) ->
           [hrefPrimBadge (showControllerURL "Category"
                          ("list" : map snd (take n parentcats) ++ [a]))
                 [htxt cname], htxt " >> "])
         (zip [0..] (init parentcats))),
   h1 [htxt (categoryName currentcat)],
   spTable (map listCategory (sortBy leqCategory categorys) ++
            map (recipeToListView (map snd parentcats))
                (sortBy leqRecipe recipes))] ++
   if isAdminSession sinfo
   then [par [hrefPrimSmButton (showControllerURL "Recipe"
                            ["new", showCategoryKey currentcat])
                         [htxt "Neues Rezept"], nbsp,
              hrefPrimSmButton (showControllerURL "Recipe"
                            ["newref", showCategoryKey currentcat])
                         [htxt "Neue Rezeptreferenz"], nbsp,
              hrefPrimSmButton (showControllerURL "Recipe"
                            ["add", showCategoryKey currentcat])
                         [htxt "Vorhandenes Rezept hinzufügen"], nbsp,
              hrefPrimSmButton (showControllerURL "Category"
                            ["new", showCategoryKey currentcat])
                         [htxt "Neue Kategorie hinzufügen"]
             ]]
   else []
  where
    listCategory :: Category -> [[BaseHtml]]
    listCategory category =
      categoryToListView (map snd parentcats) category
       ++ (if isAdminSession sinfo
             then [[hrefPrimBadge (editRoute   category) [htxt "Ändern"]],
                   [hrefPrimBadge (deleteRoute category) [htxt "Löschen"]]]
             else [])
