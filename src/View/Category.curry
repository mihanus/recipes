module View.Category
  ( wCategory, tuple2Category, category2Tuple, wCategoryType
  , blankCategoryView, createCategoryView, editCategoryView, showCategoryView
  , listCategoryView ) where

import WUI
import HTML.Base
import Time
import Sort
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

--- Supplies a WUI form to create a new Category entity.
--- The fields of the entity have some default values.
blankCategoryView
  :: UserSessionInfo
  -> ((String,Int) -> Controller) -> Controller -> [HtmlExp]
blankCategoryView sinfo controller cancelcontroller =
  createCategoryView sinfo "" 0 controller cancelcontroller

--- Supplies a WUI form to create a new Category entity.
--- Takes default values to be prefilled in the form fields.
createCategoryView
  :: UserSessionInfo
  -> String
  -> Int
  -> ((String,Int) -> Controller) -> Controller -> [HtmlExp]
createCategoryView
    _
    defaultName
    defaultPosition
    controller
    cancelcontroller =
  renderWuiForm wCategory
   (defaultName,defaultPosition)
   controller
   cancelcontroller
   "Neue Kategorie"
   "Speichern"

--- Supplies a WUI form to edit the given Category entity.
--- Takes also associated entities and a list of possible associations
--- for every associated entity type.
editCategoryView
  :: UserSessionInfo
  -> Category
  -> (Category -> Controller) -> Controller -> [HtmlExp]
editCategoryView _ category controller cancelcontroller =
  renderWuiForm (wCategoryType category) category
   controller
   cancelcontroller
   "Kategorie ändern"
   "Speichern"

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
 :: UserSessionInfo -> [(String,String)] -> String -> [Category] -> [Recipe]
  -> (Category -> Controller) -> (Category -> Controller)
  -> (Category -> Controller) -> Controller -> Controller
  -> Controller -> Controller ->  [HtmlExp]
listCategoryView sinfo parentcats catname categorys recipes _
   editCategoryController _
   newCategoryController newRecipeDescController newRecipeController
   addRecipeController =
  [h4 (concatMap
         (\ (n,(cname,a)) ->
           [hrefButton (showControllerURL "Category"
                          ("list" : map snd (take n parentcats) ++ [a]))
                 [htxt cname], htxt " >> "])
         (zip [0..] (dropLast parentcats))),
   h1 [htxt catname],
   spTable (map listCategory (sortBy leqCategory categorys) ++
            map (recipeToListView (map snd parentcats))
                (sortBy leqRecipe recipes))] ++
   if isAdminSession sinfo
   then [par [defaultButton "Neue Kategorie" (nextController newCategoryController),
              defaultButton "Neues Rezept" (nextController newRecipeDescController),
              defaultButton "Neue Rezeptreferenz"
                       (nextController newRecipeController),
              defaultButton "Vorhandenes Rezept hinzufuegen"
                       (nextController addRecipeController)]]
   else []
  where
    dropLast l = take (length l - 1) l

    listCategory :: Category -> [[HtmlExp]]
    listCategory category =
      categoryToListView (map snd parentcats) category
       ++ (if isAdminSession sinfo
             then [[hrefButton
                      ("?Category/edit/" ++ showCategoryKey category)
                      [htxt "Ändern"]]]
             else [])
