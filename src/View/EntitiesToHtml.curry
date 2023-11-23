module View.EntitiesToHtml where

import Data.Time
import HTML.Base
import HTML.WUI
import HTML.Styles.Bootstrap4
import System.Spicey
import Model.Recipes

--- The list view of a Category entity in HTML format.
--- This view is used in a row of a table of all entities.
categoryToListView :: HTML h => [String] -> Category -> [[h]]
categoryToListView parcatids cat =
  [[hrefPrimSmBlock
     (showControllerURL "Category" ("list":parcatids++[showCategoryKey cat]))
     [stringToHtml (categoryName cat)]]]

--- The short view of a Category entity as a string.
--- This view is used in menus and comments to refer to a Category entity.
categoryToShortView :: Category -> String
categoryToShortView category = categoryName category

--- The detailed view of a Category entity in HTML format.
--- It also takes associated entities for every associated entity type.
categoryToDetailsView :: HTML h => Category -> [Recipe] -> [h]
categoryToDetailsView category recipes =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip categoryLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (categoryName category)]
      ,[intToHtml (categoryPosition category)]
      ,[htxt (unwords (map recipeToShortView recipes))]]

--- The labels of a Category entity, as used in HTML tables.
categoryLabelList :: HTML h => [[h]]
categoryLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_int" "Position"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Recipe"]]

--- The list view of a Keyword entity in HTML format.
--- This view is used in a row of a table of all entities.
keywordToListView :: HTML h => Keyword -> [[h]]
keywordToListView keyword =
  [[hrefPrimBadge
      (showControllerURL "Keyword" ["show",showKeywordKey keyword])
      [stringToHtml (keywordName keyword)]]]

--- The short view of a Keyword entity as a string.
--- This view is used in menus and comments to refer to a Keyword entity.
keywordToShortView :: Keyword -> String
keywordToShortView keyword = keywordName keyword

--- The detailed view of a Keyword entity in HTML format.
keywordToDetailsView :: HTML h => Keyword -> [h]
keywordToDetailsView keyword =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip keywordLabelList detailedView))]
  where
    detailedView = [[stringToHtml (keywordName keyword)]]

--- The labels of a Keyword entity, as used in HTML tables.
keywordLabelList :: HTML h => [[h]]
keywordLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]]

--- The list view of a Recipe entity in HTML format.
--- This view is used in a row of a table of all entities.
recipeToListView :: HTML h => [String] -> Recipe -> [[h]]
recipeToListView parcatids recipe =
  [[hrefInfoSmBlock
      (showControllerURL "Recipe" ("show": showRecipeKey recipe : parcatids))
      [stringToHtml (recipeName recipe)]]]

--- The list view of a Recipe entity and a tag indicating
--- whether this Recipe has a description in HTML format.
--- This view is used in a row of a table of all entities.
recipeDescToListView :: HTML h => [String] -> (Recipe,Bool) -> [[h]]
recipeDescToListView parcatids (recipe,hasdesc) =
  [[(if hasdesc then hrefPrimSmBlock else hrefInfoSmBlock)
      (showControllerURL "Recipe" ("show": showRecipeKey recipe : parcatids))
      [stringToHtml (recipeName recipe)]]]

--- The list view of the reference of a Recipe entity in HTML format.
recipeRefToListView :: HTML h => [String] -> Recipe -> [[h]]
recipeRefToListView parcatids recipe =
  [[hrefInfoSmBlock
      (showControllerURL "Recipe" ("show": showRecipeKey recipe : parcatids))
      [stringToHtml (recipeReference recipe)]]]

--- The short view of a Recipe entity as a string.
--- This view is used in menus and comments to refer to a Recipe entity.
recipeToShortView :: Recipe -> String
recipeToShortView recipe = recipeName recipe

--- The detailed view of a Recipe entity in HTML format.
--- It also takes associated entities for every associated entity type.
recipeToDetailsView :: HTML h => Recipe -> [Keyword] -> [h]
recipeToDetailsView recipe keywords =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip recipeLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (recipeName recipe)]
      ,[stringToHtml (recipeReference recipe)]
      ,[htxt (unwords (map keywordToShortView keywords))]]

--- The labels of a Recipe entity, as used in HTML tables.
recipeLabelList :: HTML h => [[h]]
recipeLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Name"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Reference"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Keyword"]]

--- The list view of a RecipeDescription entity in HTML format.
--- This view is used in a row of a table of all entities.
recipeDescriptionToListView :: HTML h => RecipeDescription -> [[h]]
recipeDescriptionToListView recipeDescription =
  [[stringToHtml (recipeDescriptionServings recipeDescription)]
  ,[stringToHtml (recipeDescriptionIngredients recipeDescription)]
  ,[stringToHtml (recipeDescriptionDirections recipeDescription)]
  ,[stringToHtml (recipeDescriptionPrepTime recipeDescription)]
  ,[stringToHtml (recipeDescriptionCookTime recipeDescription)]]

--- The short view of a RecipeDescription entity as a string.
--- This view is used in menus and comments to refer to a RecipeDescription entity.
recipeDescriptionToShortView :: RecipeDescription -> String
recipeDescriptionToShortView recipeDescription =
  recipeDescriptionServings recipeDescription

--- The detailed view of a RecipeDescription entity in HTML format.
--- It also takes associated entities for every associated entity type.
recipeDescriptionToDetailsView :: HTML h => RecipeDescription -> Recipe -> [h]
recipeDescriptionToDetailsView recipeDescription relatedRecipe =
  [spTable
    (map (\(label,value) -> [label,value])
      (zip recipeDescriptionLabelList detailedView))]
  where
    detailedView =
      [[stringToHtml (recipeDescriptionServings recipeDescription)]
      ,[stringToHtml (recipeDescriptionIngredients recipeDescription)]
      ,[stringToHtml (recipeDescriptionDirections recipeDescription)]
      ,[stringToHtml (recipeDescriptionPrepTime recipeDescription)]
      ,[stringToHtml (recipeDescriptionCookTime recipeDescription)]
      ,[htxt (recipeToShortView relatedRecipe)]]

--- The labels of a RecipeDescription entity, as used in HTML tables.
recipeDescriptionLabelList :: HTML h => [[h]]
recipeDescriptionLabelList =
  [[textstyle "spicey_label spicey_label_for_type_string" "Portionen"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Zutaten"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Anleitung"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Vorbereitungszeit (Minuten)"]
  ,[textstyle "spicey_label spicey_label_for_type_string" "Garzeit (Minuten)"]
  ,[textstyle "spicey_label spicey_label_for_type_relation" "Rezept"]]
