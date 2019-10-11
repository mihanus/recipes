module View.Category
  ( wCategory, tuple2Category, category2Tuple, wCategoryType
  , showCategoryView, listCategoryView
  ) where

import List       ( init )
import Sort
import Time

import HTML.Base
import HTML.WUI
import HTML.Styles.Bootstrap3
import System.Authentication
import System.Spicey
import System.SessionInfo
import Recipes
import View.RecipesEntitiesToHtml
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
showCategoryView :: UserSessionInfo -> Category -> [Recipe] -> [HtmlExp]
showCategoryView _ category recipes =
  categoryToDetailsView category recipes
   ++ [hrefButton "?Category/list" [htxt "back to Category list"]]

--- Compares two Category entities. This order is used in the list view.
leqCategory :: Category -> Category -> Bool
leqCategory x1 x2 = categoryPosition x1 <= categoryPosition x2

--- Supplies a list view for a given list of Category entities.
--- Shows also buttons to show, delete, or edit entries.
--- The arguments are the name of the category, the list of Category entities,
--- and the controller functions to show, delete and edit entities.
listCategoryView
 :: UserSessionInfo -> [(String,String)] -> Category -> [Category] -> [Recipe]
  -> [HtmlExp]
listCategoryView sinfo parentcats currentcat categorys recipes =
  [h4 (concatMap
         (\ (n,(cname,a)) ->
           [hrefButton (showControllerURL "Category"
                          ("list" : map snd (take n parentcats) ++ [a]))
                 [htxt cname], htxt " >> "])
         (zip [0..] (init parentcats))),
   h1 [htxt (categoryName currentcat)],
   spTable (map listCategory (sortBy leqCategory categorys) ++
            map (recipeToListView (map snd parentcats))
                (sortBy leqRecipe recipes))] ++
   if isAdminSession sinfo
   then [par [hrefButton (showControllerURL "Recipe"
                            ["new", showCategoryKey currentcat])
                         [htxt "Neues Rezept"], nbsp,
              hrefButton (showControllerURL "Recipe"
                            ["newref", showCategoryKey currentcat])
                         [htxt "Neue Rezeptreferenz"], nbsp,
              hrefButton (showControllerURL "Recipe"
                            ["add", showCategoryKey currentcat])
                         [htxt "Vorhandenes Rezept hinzufügen"], nbsp,
              hrefButton (showControllerURL "Category"
                            ["new", showCategoryKey currentcat])
                         [htxt "Neue Kategorie hinzufügen"]
             ]]
   else []
  where
    listCategory :: Category -> [[HtmlExp]]
    listCategory category =
      categoryToListView (map snd parentcats) category
       ++ (if isAdminSession sinfo
             then [[hrefSmallButton
                      ("?Category/edit/" ++ showCategoryKey category)
                      [htxt "Ändern"]],
                   [hrefSmallButton
                      ("?Category/delete/" ++ showCategoryKey category)
                      [htxt "Löschen"]]]
             else [])
