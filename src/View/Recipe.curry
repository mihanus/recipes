module View.Recipe
  ( keywords2string, string2keywords
  , wRecipe, tuple2Recipe, recipe2Tuple, wRecipeType, wRecipeDesc
  , showRecipeView, listRecipeView
  , listRecipesOfKeyword, singleRecipeView, leqRecipe ) where

import Char ( isSpace )
import Global
import List ( intersperse, isSuffixOf, last, split )
import Sort
import Time

import Config.Storage
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.Session
import HTML.WUI
import System.Authentication
import System.Spicey
import System.SessionInfo
import Recipes
import View.EntitiesToHtml

--- Shows keywords as comma-separated string.
keywords2string :: [Keyword] -> String
keywords2string keywords = concat (intersperse ", " (map keywordName keywords))

--- Transform a comma-separated keyword list into the keyword names.
string2keywords :: String -> [String]
string2keywords = filter (not . null) . map strip . words
 where
  words :: String -> [String]
  words s = if null s then []
                      else let (w,s') = break (==',') s
                            in w : words (if null s' then s' else tail s')

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


--- The WUI specification for the entity type Recipe.
--- It also includes fields for associated entities.
wRecipe :: WuiSpec (String,String,String)
wRecipe =
  withRendering
   (wTriple (wRequiredStringSize 60) (wStringSize 60) (wStringSize 60))
   (renderLabels recipeLabelList)

--- Transformation from data of a WUI form to entity type Recipe.
tuple2Recipe :: Recipe
             -> (String,String,String) -> (Recipe,String,Maybe RecipeDescription)
tuple2Recipe recipeToUpdate (name ,reference ,keywords) =
  (setRecipeName (setRecipeReference recipeToUpdate reference) name,keywords,
   Nothing)

--- Transformation from entity type Recipe to a tuple
--- which can be used in WUI specifications.
recipe2Tuple :: (Recipe,String,Maybe RecipeDescription) -> (String,String,String)
recipe2Tuple (recipe ,keywords,_) =
  (recipeName recipe,recipeReference recipe,keywords)

--- The WUI specification for the entity type Recipe with a description.
--- It also includes fields for associated entities.
wRecipeDesc :: WuiSpec (String,String,String,String,String,String,String,String)
wRecipeDesc =
  withRendering
   (w8Tuple (wRequiredStringSize 60) (wStringSize 60) (wStringSize 60)
            wRequiredString
            (wRequiredTextArea (10,70)) (wRequiredTextArea (13,70))
            wString wString)
   (renderLabels (take 8 (recipeLabelList++recipeDescriptionLabelList)))

--- A widget for editing string values in a text area
--- that are required to be non-empty.
--- The argument specifies the height and width of the text area.
wRequiredTextArea :: (Int,Int) -> WuiSpec String
wRequiredTextArea dims =
  wTextArea dims `withError`     "Missing input:"
                 `withCondition` (not . null . strip)

--- Transformation from data of a WUI form to entity type Recipe.
tuple2RecipeDesc :: Recipe -> RecipeDescription
                 -> (String,String,String,String,String,String,String,String)
                 -> (Recipe,String,Maybe RecipeDescription)
tuple2RecipeDesc recipeToUpdate recdescToUpdate
  (name,reference,keywords,servings,ingredients,directions,prepTime,cookTime) =
 (setRecipeName (setRecipeReference recipeToUpdate reference) name,
  keywords,
  Just $ 
  setRecipeDescriptionServings
   (setRecipeDescriptionIngredients
     (setRecipeDescriptionDirections
       (setRecipeDescriptionPrepTime
         (setRecipeDescriptionCookTime recdescToUpdate cookTime)
         prepTime)
       directions)
     ingredients)
   servings)

--- Transformation from entity type Recipe to a tuple
--- which can be used in WUI specifications.
recipeDesc2Tuple :: (Recipe,String,Maybe RecipeDescription)
          -> (String,String,String,String,String,String,String,String)
recipeDesc2Tuple (recipe,keywords,Just recdesc) =
  (recipeName recipe,
   recipeReference recipe,
   keywords,
   recipeDescriptionServings recdesc,
   recipeDescriptionIngredients recdesc,
   recipeDescriptionDirections recdesc,
   recipeDescriptionPrepTime recdesc,
   recipeDescriptionCookTime recdesc)

--- WUI Type for editing or creating Recipe entities.
--- Includes fields for associated entities.
wRecipeType :: Recipe -> Maybe RecipeDescription
            -> WuiSpec (Recipe,String,Maybe RecipeDescription)
wRecipeType recipe Nothing =
  transformWSpec (tuple2Recipe recipe,recipe2Tuple) wRecipe
wRecipeType recipe (Just recdesc) =
  transformWSpec (tuple2RecipeDesc recipe recdesc,recipeDesc2Tuple) wRecipeDesc

--------------------------------------------------------------------------
--- Supplies a view to show the details of a Recipe.
showRecipeView :: UserSessionInfo -> Recipe -> [Keyword] -> [HtmlExp]
showRecipeView _ recipe keywords =
  recipeToDetailsView recipe keywords
   ++ [hrefPrimSmButton "?Recipe/list" [htxt "back to Recipe list"]]

--------------------------------------------------------------------------
--- Compares two Recipe entities. This order is used in the list view.
leqRecipe :: Recipe -> Recipe -> Bool
leqRecipe x1 x2 =
  (recipeName x1,recipeReference x1) <= (recipeName x2,recipeReference x2)

--- Compares two Recipe entities by their references.
--- This order is used in the list reference view.
leqRecipeRef :: Recipe -> Recipe -> Bool
leqRecipeRef x1 x2 =
  (recipeReference x1,recipeName x1) <= (recipeReference x2,recipeName x2)

--- Supplies a view for a given single Recipe entity.
singleRecipeView :: UserSessionInfo -> [(Category,String)] -> Recipe
  -> [Keyword] -> Maybe RecipeDescription -> Maybe String -> Maybe String
  -> [HtmlExp]
singleRecipeView sinfo parentcats recipe keywords mbrecdesc mbpic mbpdf =
  [h4 (concatMap
          (\ (n, (cat,a)) ->
            [hrefPrimBadge (showControllerURL "Category"
                             ("list" : map snd (take n parentcats) ++[a]))
                           [htxt (categoryName cat)], htxt " >> "])
          (zip [0..] parentcats)),
   h1 $ [htxt $ recipeName recipe] ++
        maybe [] (\pdf -> [htxt " (", href pdf [htxt "PDF"], htxt ")"]) mbpdf,
   h4 [htxt ("Stichworte: " ++ keywords2string keywords)]] ++
  (maybe [] (\pic -> [par [imageIcon pic]]) mbpic) ++
  (let ref = recipeReference recipe
   in if null ref then [] else [h4 $ recipeReference2HTML ref]) ++
  [par (showRecipeDescription mbrecdesc)] ++
  if isAdminSession sinfo
   then
   [par [hrefPrimSmButton (showControllerURL "Recipe"
                       ("edit" : showRecipeKey recipe : map snd parentcats))
                    [htxt "Ändern"], nbsp,
         hrefPrimSmButton (showControllerURL "Recipe"
                       ["uploadpic", showRecipeKey recipe])
           [htxt $ "Bild " ++ maybe "hinzufügen" (const "ändern") mbpic], nbsp,
         hrefPrimSmButton (showControllerURL "Recipe"
                       ["uploadpdf", showRecipeKey recipe])
           [htxt $ "PDF " ++ maybe "hinzufügen" (const "ändern") mbpdf], nbsp,
         hrefPrimSmButton (showControllerURL "Recipe"
                       ("delete" : showRecipeKey recipe :
                        maybe [] (\c -> [snd c]) currentCat))
                    [htxt $ maybe "Rezept" (const "in Kategorie") currentCat ++
                            " löschen"]
        ]]
   else []
 where
  currentCat = if null parentcats
                 then Nothing
                 else Just (last parentcats)

  imageIcon src = href src
    [image src "Bild" `addAttr` ("width","400")
       `addClass` "img-responsive img-rounded center-block"]

-- Shows the reference of a recipe. If it contains the substring "<....pdf>",
-- it will be shown as a reference to the `recipes_archives` directory.
recipeReference2HTML :: String -> [HtmlExp]
recipeReference2HTML ref =
  (htxt "in: ") :
  let (prefix,latxt) = break (=='<') ref
   in if null latxt
        then [htxt ref]
        else let (pdfref,suffix) = break (=='>') (tail latxt) in
              if null suffix || not (".pdf" `isSuffixOf` pdfref)
                then [htxt ref]
                else [htxt prefix, htxt "(",
                      bold [href ("../recipes_archive/" ++ pdfref)
                                 [htxt "PDF"]],
                      htxt ")", htxt (tail suffix)]

showRecipeDescription :: Maybe RecipeDescription -> [HtmlExp]
showRecipeDescription Nothing = []
showRecipeDescription (Just recdesc) =
  [h4 [htxt $ "Portionen: " ++ recipeDescriptionServings recdesc],
   h4 [htxt "Zutaten:"],
   par (intersperse breakline
          (map stringToHtml (lines (recipeDescriptionIngredients recdesc)))),
   h4 [htxt "Anleitung:"]] ++
  linesToHtmlPars (recipeDescriptionDirections recdesc) ++
  [par [stringToHtml $ "Vorbereitungszeit (Minuten): " ++
                       recipeDescriptionPrepTime recdesc, breakline,
        stringToHtml $ "Garzeit (Minuten): " ++
                       recipeDescriptionCookTime recdesc]]

-- Transforms a string into HTML paragraphs, where each
-- blank line starts a new paragraph.
linesToHtmlPars :: String -> [HtmlExp]
linesToHtmlPars =
  map (\s -> par [htxt s]) . map unlines . split (all isSpace) . lines

--- Supplies a list view for a given list of Recipe entities.
listRecipeView :: UserSessionInfo -> String -> [Recipe] -> [HtmlExp]
listRecipeView _ title recipes =
  [h1 [htxt title]
  ,spTable (map listRecipe (sortBy leqRecipe recipes))]
  where listRecipe :: Recipe -> [[HtmlExp]]
        listRecipe recipe = recipeToListView [] recipe

--- Supplies a list view for a given list of recipes of a keyword.
listRecipesOfKeyword :: UserSessionInfo -> Keyword -> [Recipe] -> [HtmlExp]
listRecipesOfKeyword sinfo keyword recipes =
  [h1 [htxt ("Rezepte mit Stichwort: " ++ keywordName keyword)]
  ,spTable (map (recipeToListView []) (sortBy leqRecipe recipes))] ++
  if null recipes && isAdminSession sinfo
    then [hrefInfoBlock
            (showControllerURL "Keyword" ["delete", showKeywordKey keyword])
            [stringToHtml "Stichwort löschen"]]
    else []

--- Supplies a list view to show the references contained in
--- a given list of Recipe entities.
listRecipeRefView :: [Recipe] -> [HtmlExp]
listRecipeRefView recipes =
  [h1 [htxt "Rezeptreferenzliste"]
  ,spTable (map listRecipe
                (sortBy leqRecipeRef (filter hasReference recipes)))]
  where
   hasReference recipe = not (null (recipeReference recipe))

   listRecipe :: Recipe -> [[HtmlExp]]
   listRecipe recipe = recipeRefToListView [] recipe
