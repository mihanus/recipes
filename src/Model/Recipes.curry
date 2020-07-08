--- This file has been generated from
--- 
---     /home/mh/home/curry/applications/recipes/Recipes.erdterm
--- 
--- and contains definitions for all entities and relations
--- specified in this model.

module Recipes where

import qualified Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

data Tagging = Tagging RecipeID KeywordID
 deriving (Eq,Show,Read)

data TaggingID = TaggingID Int
 deriving (Eq,Show,Read)

data RecipeCategory = RecipeCategory CategoryID RecipeID
 deriving (Eq,Show,Read)

data RecipeCategoryID = RecipeCategoryID Int
 deriving (Eq,Show,Read)

data Category = Category CategoryID String Int (Maybe CategoryID)
 deriving (Eq,Show,Read)

data CategoryID = CategoryID Int
 deriving (Eq,Show,Read)

data Keyword = Keyword KeywordID String
 deriving (Eq,Show,Read)

data KeywordID = KeywordID Int
 deriving (Eq,Show,Read)

data Recipe = Recipe RecipeID String String
 deriving (Eq,Show,Read)

data RecipeID = RecipeID Int
 deriving (Eq,Show,Read)

data RecipeDescription = RecipeDescription RecipeDescriptionID String String String String String RecipeID
 deriving (Eq,Show,Read)

data RecipeDescriptionID = RecipeDescriptionID Int
 deriving (Eq,Show,Read)

--- The name of the SQLite database file.
sqliteDBFile :: String
sqliteDBFile = "/net/medoc/home/mh/home/data/recipes/Recipes.db"

--- The ER description of the `Tagging` entity.
tagging_CDBI_Description :: Database.CDBI.Description.EntityDescription Tagging
tagging_CDBI_Description =
  Database.CDBI.Description.ED "Tagging"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(Tagging (RecipeID recipeTaggingKey) (KeywordID keywordTaggingKey)) ->
     [Database.CDBI.Connection.SQLInt recipeTaggingKey
     ,Database.CDBI.Connection.SQLInt keywordTaggingKey])
   (\(Tagging (RecipeID recipeTaggingKey) (KeywordID keywordTaggingKey)) ->
     [Database.CDBI.Connection.SQLInt recipeTaggingKey
     ,Database.CDBI.Connection.SQLInt keywordTaggingKey])
   (\[Database.CDBI.Connection.SQLInt recipeTaggingKey
     ,Database.CDBI.Connection.SQLInt keywordTaggingKey] ->
     Tagging (RecipeID recipeTaggingKey) (KeywordID keywordTaggingKey))

--- The database table of the `Tagging` entity.
taggingTable :: Database.CDBI.Description.Table
taggingTable = "Tagging"

--- The database column `RecipeTaggingKey` of the `Tagging` entity.
taggingColumnRecipeTaggingKey :: Database.CDBI.Description.Column RecipeID
taggingColumnRecipeTaggingKey =
  Database.CDBI.Description.Column "\"RecipeTaggingKey\""
   "\"Tagging\".\"RecipeTaggingKey\""

--- The database column `KeywordTaggingKey` of the `Tagging` entity.
taggingColumnKeywordTaggingKey :: Database.CDBI.Description.Column KeywordID
taggingColumnKeywordTaggingKey =
  Database.CDBI.Description.Column "\"KeywordTaggingKey\""
   "\"Tagging\".\"KeywordTaggingKey\""

--- The description of the database column `RecipeTaggingKey` of the `Tagging` entity.
taggingRecipeTaggingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription RecipeID
taggingRecipeTaggingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Tagging\".\"RecipeTaggingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(RecipeID recipeTaggingKey) ->
     Database.CDBI.Connection.SQLInt recipeTaggingKey)
   (\(Database.CDBI.Connection.SQLInt recipeTaggingKey) ->
     RecipeID recipeTaggingKey)

--- The description of the database column `KeywordTaggingKey` of the `Tagging` entity.
taggingKeywordTaggingKeyColDesc
  :: Database.CDBI.Description.ColumnDescription KeywordID
taggingKeywordTaggingKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Tagging\".\"KeywordTaggingKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(KeywordID keywordTaggingKey) ->
     Database.CDBI.Connection.SQLInt keywordTaggingKey)
   (\(Database.CDBI.Connection.SQLInt keywordTaggingKey) ->
     KeywordID keywordTaggingKey)

--- Gets the attribute `RecipeTaggingKey` of the `Tagging` entity.
taggingRecipeTaggingKey :: Tagging -> RecipeID
taggingRecipeTaggingKey (Tagging a _) = a

--- Gets the attribute `KeywordTaggingKey` of the `Tagging` entity.
taggingKeywordTaggingKey :: Tagging -> KeywordID
taggingKeywordTaggingKey (Tagging _ a) = a

--- Sets the attribute `RecipeTaggingKey` of the `Tagging` entity.
setTaggingRecipeTaggingKey :: Tagging -> RecipeID -> Tagging
setTaggingRecipeTaggingKey (Tagging _ b1) a = Tagging a b1

--- Sets the attribute `KeywordTaggingKey` of the `Tagging` entity.
setTaggingKeywordTaggingKey :: Tagging -> KeywordID -> Tagging
setTaggingKeywordTaggingKey (Tagging a2 _) a = Tagging a2 a

--- Inserts a new `Tagging` relation.
newTagging :: RecipeID -> KeywordID -> Database.CDBI.Connection.DBAction ()
newTagging k1 k2 =
  Database.CDBI.ER.insertEntry tagging_CDBI_Description (Tagging k1 k2)

--- Deletes an existing `Tagging` relation.
deleteTagging :: RecipeID -> KeywordID -> Database.CDBI.Connection.DBAction ()
deleteTagging k1 k2 =
  Database.CDBI.ER.deleteEntryR tagging_CDBI_Description
   taggingColumnRecipeTaggingKey
   (recipeID k1)
   taggingColumnKeywordTaggingKey
   (keywordID k2)

--- Gets the associated `Recipe` entities for a given `Keyword` entity
--- w.r.t. the `Tagging` relation.
getRecipeKeywords :: Recipe -> Database.CDBI.Connection.DBAction [Keyword]
getRecipeKeywords en =
  Database.CDBI.ER.getEntriesWithColVal tagging_CDBI_Description
   taggingColumnRecipeTaggingKey
   (recipeID (recipeKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getKeyword (map taggingKeywordTaggingKey vals))

--- The ER description of the `RecipeCategory` entity.
recipeCategory_CDBI_Description
  :: Database.CDBI.Description.EntityDescription RecipeCategory
recipeCategory_CDBI_Description =
  Database.CDBI.Description.ED "RecipeCategory"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeInt]
   (\(RecipeCategory
       (CategoryID categoryRecipeCategoryKey)
       (RecipeID recipeRecipeCategoryKey)) ->
     [Database.CDBI.Connection.SQLInt categoryRecipeCategoryKey
     ,Database.CDBI.Connection.SQLInt recipeRecipeCategoryKey])
   (\(RecipeCategory
       (CategoryID categoryRecipeCategoryKey)
       (RecipeID recipeRecipeCategoryKey)) ->
     [Database.CDBI.Connection.SQLInt categoryRecipeCategoryKey
     ,Database.CDBI.Connection.SQLInt recipeRecipeCategoryKey])
   (\[Database.CDBI.Connection.SQLInt categoryRecipeCategoryKey
     ,Database.CDBI.Connection.SQLInt recipeRecipeCategoryKey] ->
     RecipeCategory (CategoryID categoryRecipeCategoryKey)
      (RecipeID recipeRecipeCategoryKey))

--- The database table of the `RecipeCategory` entity.
recipeCategoryTable :: Database.CDBI.Description.Table
recipeCategoryTable = "RecipeCategory"

--- The database column `CategoryRecipeCategoryKey` of the `RecipeCategory` entity.
recipeCategoryColumnCategoryRecipeCategoryKey
  :: Database.CDBI.Description.Column CategoryID
recipeCategoryColumnCategoryRecipeCategoryKey =
  Database.CDBI.Description.Column "\"CategoryRecipeCategoryKey\""
   "\"RecipeCategory\".\"CategoryRecipeCategoryKey\""

--- The database column `RecipeRecipeCategoryKey` of the `RecipeCategory` entity.
recipeCategoryColumnRecipeRecipeCategoryKey
  :: Database.CDBI.Description.Column RecipeID
recipeCategoryColumnRecipeRecipeCategoryKey =
  Database.CDBI.Description.Column "\"RecipeRecipeCategoryKey\""
   "\"RecipeCategory\".\"RecipeRecipeCategoryKey\""

--- The description of the database column `CategoryRecipeCategoryKey` of the `RecipeCategory` entity.
recipeCategoryCategoryRecipeCategoryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription CategoryID
recipeCategoryCategoryRecipeCategoryKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"RecipeCategory\".\"CategoryRecipeCategoryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID categoryRecipeCategoryKey) ->
     Database.CDBI.Connection.SQLInt categoryRecipeCategoryKey)
   (\(Database.CDBI.Connection.SQLInt categoryRecipeCategoryKey) ->
     CategoryID categoryRecipeCategoryKey)

--- The description of the database column `RecipeRecipeCategoryKey` of the `RecipeCategory` entity.
recipeCategoryRecipeRecipeCategoryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription RecipeID
recipeCategoryRecipeRecipeCategoryKeyColDesc =
  Database.CDBI.Description.ColDesc
   "\"RecipeCategory\".\"RecipeRecipeCategoryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(RecipeID recipeRecipeCategoryKey) ->
     Database.CDBI.Connection.SQLInt recipeRecipeCategoryKey)
   (\(Database.CDBI.Connection.SQLInt recipeRecipeCategoryKey) ->
     RecipeID recipeRecipeCategoryKey)

--- Gets the attribute `CategoryRecipeCategoryKey` of the `RecipeCategory` entity.
recipeCategoryCategoryRecipeCategoryKey :: RecipeCategory -> CategoryID
recipeCategoryCategoryRecipeCategoryKey (RecipeCategory a _) = a

--- Gets the attribute `RecipeRecipeCategoryKey` of the `RecipeCategory` entity.
recipeCategoryRecipeRecipeCategoryKey :: RecipeCategory -> RecipeID
recipeCategoryRecipeRecipeCategoryKey (RecipeCategory _ a) = a

--- Sets the attribute `CategoryRecipeCategoryKey` of the `RecipeCategory` entity.
setRecipeCategoryCategoryRecipeCategoryKey
  :: RecipeCategory -> CategoryID -> RecipeCategory
setRecipeCategoryCategoryRecipeCategoryKey (RecipeCategory _ b1) a =
  RecipeCategory a b1

--- Sets the attribute `RecipeRecipeCategoryKey` of the `RecipeCategory` entity.
setRecipeCategoryRecipeRecipeCategoryKey
  :: RecipeCategory -> RecipeID -> RecipeCategory
setRecipeCategoryRecipeRecipeCategoryKey (RecipeCategory a2 _) a =
  RecipeCategory a2 a

--- Inserts a new `RecipeCategory` relation.
newRecipeCategory
  :: CategoryID -> RecipeID -> Database.CDBI.Connection.DBAction ()
newRecipeCategory k1 k2 =
  Database.CDBI.ER.insertEntry recipeCategory_CDBI_Description
   (RecipeCategory k1 k2)

--- Deletes an existing `RecipeCategory` relation.
deleteRecipeCategory
  :: CategoryID -> RecipeID -> Database.CDBI.Connection.DBAction ()
deleteRecipeCategory k1 k2 =
  Database.CDBI.ER.deleteEntryR recipeCategory_CDBI_Description
   recipeCategoryColumnCategoryRecipeCategoryKey
   (categoryID k1)
   recipeCategoryColumnRecipeRecipeCategoryKey
   (recipeID k2)

--- Gets the associated `Category` entities for a given `Recipe` entity
--- w.r.t. the `RecipeCategory` relation.
getCategoryRecipes :: Category -> Database.CDBI.Connection.DBAction [Recipe]
getCategoryRecipes en =
  Database.CDBI.ER.getEntriesWithColVal recipeCategory_CDBI_Description
   recipeCategoryColumnCategoryRecipeCategoryKey
   (categoryID (categoryKey en))
   Database.CDBI.ER.>+= (\vals ->
     mapM getRecipe (map recipeCategoryRecipeRecipeCategoryKey vals))

--- The ER description of the `Category` entity.
category_CDBI_Description
  :: Database.CDBI.Description.EntityDescription Category
category_CDBI_Description =
  Database.CDBI.Description.ED "Category"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(Category (CategoryID key) name position categoryParentCategoryKey) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLInt position
     ,Database.CDBI.Description.sqlKeyOrNull (\(CategoryID k) -> k)
       categoryParentCategoryKey])
   (\(Category _ name position categoryParentCategoryKey) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLInt position
     ,Database.CDBI.Description.sqlKeyOrNull (\(CategoryID k) -> k)
       categoryParentCategoryKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLInt position
     ,categoryParentCategoryKey] ->
     Category (CategoryID key) name position
      (Database.CDBI.Description.keyOrNothing CategoryID
        categoryParentCategoryKey))

--- The database table of the `Category` entity.
categoryTable :: Database.CDBI.Description.Table
categoryTable = "Category"

--- The database column `Key` of the `Category` entity.
categoryColumnKey :: Database.CDBI.Description.Column CategoryID
categoryColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Category\".\"Key\""

--- The database column `Name` of the `Category` entity.
categoryColumnName :: Database.CDBI.Description.Column String
categoryColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Category\".\"Name\""

--- The database column `Position` of the `Category` entity.
categoryColumnPosition :: Database.CDBI.Description.Column Int
categoryColumnPosition =
  Database.CDBI.Description.Column "\"Position\"" "\"Category\".\"Position\""

--- The database column `CategoryParentCategoryKey` of the `Category` entity.
categoryColumnCategoryParentCategoryKey
  :: Database.CDBI.Description.Column CategoryID
categoryColumnCategoryParentCategoryKey =
  Database.CDBI.Description.Column "\"CategoryParentCategoryKey\""
   "\"Category\".\"CategoryParentCategoryKey\""

--- The description of the database column `Key` of the `Category` entity.
categoryKeyColDesc :: Database.CDBI.Description.ColumnDescription CategoryID
categoryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(CategoryID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> CategoryID key)

--- The description of the database column `Name` of the `Category` entity.
categoryNameColDesc :: Database.CDBI.Description.ColumnDescription String
categoryNameColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Position` of the `Category` entity.
categoryPositionColDesc :: Database.CDBI.Description.ColumnDescription Int
categoryPositionColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"Position\""
   Database.CDBI.Connection.SQLTypeInt
   (\position -> Database.CDBI.Connection.SQLInt position)
   (\(Database.CDBI.Connection.SQLInt position) -> position)

--- The description of the database column `CategoryParentCategoryKey` of the `Category` entity.
categoryCategoryParentCategoryKeyColDesc
  :: Database.CDBI.Description.ColumnDescription (Maybe CategoryID)
categoryCategoryParentCategoryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Category\".\"CategoryParentCategoryKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\categoryParentCategoryKey ->
     Database.CDBI.Description.sqlKeyOrNull (\(CategoryID k) -> k)
      categoryParentCategoryKey)
   (\categoryParentCategoryKey ->
     Database.CDBI.Description.keyOrNothing CategoryID
      categoryParentCategoryKey)

--- Gets the attribute `Key` of the `Category` entity.
categoryKey :: Category -> CategoryID
categoryKey (Category a _ _ _) = a

--- Gets the attribute `Name` of the `Category` entity.
categoryName :: Category -> String
categoryName (Category _ a _ _) = a

--- Gets the attribute `Position` of the `Category` entity.
categoryPosition :: Category -> Int
categoryPosition (Category _ _ a _) = a

--- Gets the attribute `CategoryParentCategoryKey` of the `Category` entity.
categoryCategoryParentCategoryKey :: Category -> Maybe CategoryID
categoryCategoryParentCategoryKey (Category _ _ _ a) = a

--- Sets the attribute `Key` of the `Category` entity.
setCategoryKey :: Category -> CategoryID -> Category
setCategoryKey (Category _ b3 b2 b1) a = Category a b3 b2 b1

--- Sets the attribute `Name` of the `Category` entity.
setCategoryName :: Category -> String -> Category
setCategoryName (Category a2 _ b2 b1) a = Category a2 a b2 b1

--- Sets the attribute `Position` of the `Category` entity.
setCategoryPosition :: Category -> Int -> Category
setCategoryPosition (Category a3 a2 _ b1) a = Category a3 a2 a b1

--- Sets the attribute `CategoryParentCategoryKey` of the `Category` entity.
setCategoryCategoryParentCategoryKey :: Category -> Maybe CategoryID -> Category
setCategoryCategoryParentCategoryKey (Category a4 a3 a2 _) a =
  Category a4 a3 a2 a

--- id-to-value function for entity `Category`.
categoryID :: CategoryID -> Database.CDBI.Criteria.Value CategoryID
categoryID (CategoryID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Category`.
categoryKeyToInt :: CategoryID -> Int
categoryKeyToInt (CategoryID key) = key

--- Shows the key of a `Category` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showCategoryKey :: Category -> String
showCategoryKey entry =
  Database.CDBI.ER.showDatabaseKey "Category" categoryKeyToInt
   (categoryKey entry)

--- Transforms a string into a key of a `Category` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readCategoryKey :: String -> Maybe CategoryID
readCategoryKey = Database.CDBI.ER.readDatabaseKey "Category" CategoryID

--- Gets all `Category` entities.
queryAllCategorys :: Database.CDBI.Connection.DBAction [Category]
queryAllCategorys = Database.CDBI.ER.getAllEntries category_CDBI_Description

--- Gets all `Category` entities satisfying a given predicate.
queryCondCategory
  :: (Category -> Bool) -> Database.CDBI.Connection.DBAction [Category]
queryCondCategory = Database.CDBI.ER.getCondEntries category_CDBI_Description

--- Gets a `Category` entry by a given key.
getCategory :: CategoryID -> Database.CDBI.Connection.DBAction Category
getCategory =
  Database.CDBI.ER.getEntryWithKey category_CDBI_Description categoryColumnKey
   categoryID

--- Inserts a new `Category` entity.
newCategoryWithCategoryParentCategoryKey
  :: String
  -> Int -> Maybe CategoryID -> Database.CDBI.Connection.DBAction Category
newCategoryWithCategoryParentCategoryKey
    name_p position_p categoryParentCategoryKey_p =
  Database.CDBI.ER.insertNewEntry category_CDBI_Description setCategoryKey
   CategoryID
   (Category (CategoryID 0) name_p position_p categoryParentCategoryKey_p)

--- Deletes an existing `Category` entry by its key.
deleteCategory :: Category -> Database.CDBI.Connection.DBAction ()
deleteCategory =
  Database.CDBI.ER.deleteEntry category_CDBI_Description categoryColumnKey
   (categoryID . categoryKey)

--- Updates an existing `Category` entry by its key.
updateCategory :: Category -> Database.CDBI.Connection.DBAction ()
updateCategory = Database.CDBI.ER.updateEntry category_CDBI_Description

--- The ER description of the `Keyword` entity.
keyword_CDBI_Description :: Database.CDBI.Description.EntityDescription Keyword
keyword_CDBI_Description =
  Database.CDBI.Description.ED "Keyword"
   [Database.CDBI.Connection.SQLTypeInt,Database.CDBI.Connection.SQLTypeString]
   (\(Keyword (KeywordID key) name) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name])
   (\(Keyword _ name) ->
     [Database.CDBI.Connection.SQLNull,Database.CDBI.Connection.SQLString name])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name] ->
     Keyword (KeywordID key) name)

--- The database table of the `Keyword` entity.
keywordTable :: Database.CDBI.Description.Table
keywordTable = "Keyword"

--- The database column `Key` of the `Keyword` entity.
keywordColumnKey :: Database.CDBI.Description.Column KeywordID
keywordColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Keyword\".\"Key\""

--- The database column `Name` of the `Keyword` entity.
keywordColumnName :: Database.CDBI.Description.Column String
keywordColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Keyword\".\"Name\""

--- The description of the database column `Key` of the `Keyword` entity.
keywordKeyColDesc :: Database.CDBI.Description.ColumnDescription KeywordID
keywordKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Keyword\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(KeywordID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> KeywordID key)

--- The description of the database column `Name` of the `Keyword` entity.
keywordNameColDesc :: Database.CDBI.Description.ColumnDescription String
keywordNameColDesc =
  Database.CDBI.Description.ColDesc "\"Keyword\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- Gets the attribute `Key` of the `Keyword` entity.
keywordKey :: Keyword -> KeywordID
keywordKey (Keyword a _) = a

--- Gets the attribute `Name` of the `Keyword` entity.
keywordName :: Keyword -> String
keywordName (Keyword _ a) = a

--- Sets the attribute `Key` of the `Keyword` entity.
setKeywordKey :: Keyword -> KeywordID -> Keyword
setKeywordKey (Keyword _ b1) a = Keyword a b1

--- Sets the attribute `Name` of the `Keyword` entity.
setKeywordName :: Keyword -> String -> Keyword
setKeywordName (Keyword a2 _) a = Keyword a2 a

--- id-to-value function for entity `Keyword`.
keywordID :: KeywordID -> Database.CDBI.Criteria.Value KeywordID
keywordID (KeywordID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Keyword`.
keywordKeyToInt :: KeywordID -> Int
keywordKeyToInt (KeywordID key) = key

--- Shows the key of a `Keyword` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showKeywordKey :: Keyword -> String
showKeywordKey entry =
  Database.CDBI.ER.showDatabaseKey "Keyword" keywordKeyToInt (keywordKey entry)

--- Transforms a string into a key of a `Keyword` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readKeywordKey :: String -> Maybe KeywordID
readKeywordKey = Database.CDBI.ER.readDatabaseKey "Keyword" KeywordID

--- Gets all `Keyword` entities.
queryAllKeywords :: Database.CDBI.Connection.DBAction [Keyword]
queryAllKeywords = Database.CDBI.ER.getAllEntries keyword_CDBI_Description

--- Gets all `Keyword` entities satisfying a given predicate.
queryCondKeyword
  :: (Keyword -> Bool) -> Database.CDBI.Connection.DBAction [Keyword]
queryCondKeyword = Database.CDBI.ER.getCondEntries keyword_CDBI_Description

--- Gets a `Keyword` entry by a given key.
getKeyword :: KeywordID -> Database.CDBI.Connection.DBAction Keyword
getKeyword =
  Database.CDBI.ER.getEntryWithKey keyword_CDBI_Description keywordColumnKey
   keywordID

--- Inserts a new `Keyword` entity.
newKeyword :: String -> Database.CDBI.Connection.DBAction Keyword
newKeyword name_p =
  Database.CDBI.ER.insertNewEntry keyword_CDBI_Description setKeywordKey
   KeywordID
   (Keyword (KeywordID 0) name_p)

--- Deletes an existing `Keyword` entry by its key.
deleteKeyword :: Keyword -> Database.CDBI.Connection.DBAction ()
deleteKeyword =
  Database.CDBI.ER.deleteEntry keyword_CDBI_Description keywordColumnKey
   (keywordID . keywordKey)

--- Updates an existing `Keyword` entry by its key.
updateKeyword :: Keyword -> Database.CDBI.Connection.DBAction ()
updateKeyword = Database.CDBI.ER.updateEntry keyword_CDBI_Description

--- The ER description of the `Recipe` entity.
recipe_CDBI_Description :: Database.CDBI.Description.EntityDescription Recipe
recipe_CDBI_Description =
  Database.CDBI.Description.ED "Recipe"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString]
   (\(Recipe (RecipeID key) name reference) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString reference])
   (\(Recipe _ name reference) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Description.sqlString reference])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,reference] ->
     Recipe (RecipeID key) name
      (Database.CDBI.Description.fromStringOrNull reference))

--- The database table of the `Recipe` entity.
recipeTable :: Database.CDBI.Description.Table
recipeTable = "Recipe"

--- The database column `Key` of the `Recipe` entity.
recipeColumnKey :: Database.CDBI.Description.Column RecipeID
recipeColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"Recipe\".\"Key\""

--- The database column `Name` of the `Recipe` entity.
recipeColumnName :: Database.CDBI.Description.Column String
recipeColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"Recipe\".\"Name\""

--- The database column `Reference` of the `Recipe` entity.
recipeColumnReference :: Database.CDBI.Description.Column String
recipeColumnReference =
  Database.CDBI.Description.Column "\"Reference\"" "\"Recipe\".\"Reference\""

--- The description of the database column `Key` of the `Recipe` entity.
recipeKeyColDesc :: Database.CDBI.Description.ColumnDescription RecipeID
recipeKeyColDesc =
  Database.CDBI.Description.ColDesc "\"Recipe\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(RecipeID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> RecipeID key)

--- The description of the database column `Name` of the `Recipe` entity.
recipeNameColDesc :: Database.CDBI.Description.ColumnDescription String
recipeNameColDesc =
  Database.CDBI.Description.ColDesc "\"Recipe\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Reference` of the `Recipe` entity.
recipeReferenceColDesc :: Database.CDBI.Description.ColumnDescription String
recipeReferenceColDesc =
  Database.CDBI.Description.ColDesc "\"Recipe\".\"Reference\""
   Database.CDBI.Connection.SQLTypeString
   (\reference -> Database.CDBI.Description.sqlString reference)
   (\reference -> Database.CDBI.Description.fromStringOrNull reference)

--- Gets the attribute `Key` of the `Recipe` entity.
recipeKey :: Recipe -> RecipeID
recipeKey (Recipe a _ _) = a

--- Gets the attribute `Name` of the `Recipe` entity.
recipeName :: Recipe -> String
recipeName (Recipe _ a _) = a

--- Gets the attribute `Reference` of the `Recipe` entity.
recipeReference :: Recipe -> String
recipeReference (Recipe _ _ a) = a

--- Sets the attribute `Key` of the `Recipe` entity.
setRecipeKey :: Recipe -> RecipeID -> Recipe
setRecipeKey (Recipe _ b2 b1) a = Recipe a b2 b1

--- Sets the attribute `Name` of the `Recipe` entity.
setRecipeName :: Recipe -> String -> Recipe
setRecipeName (Recipe a2 _ b1) a = Recipe a2 a b1

--- Sets the attribute `Reference` of the `Recipe` entity.
setRecipeReference :: Recipe -> String -> Recipe
setRecipeReference (Recipe a3 a2 _) a = Recipe a3 a2 a

--- id-to-value function for entity `Recipe`.
recipeID :: RecipeID -> Database.CDBI.Criteria.Value RecipeID
recipeID (RecipeID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `Recipe`.
recipeKeyToInt :: RecipeID -> Int
recipeKeyToInt (RecipeID key) = key

--- Shows the key of a `Recipe` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showRecipeKey :: Recipe -> String
showRecipeKey entry =
  Database.CDBI.ER.showDatabaseKey "Recipe" recipeKeyToInt (recipeKey entry)

--- Transforms a string into a key of a `Recipe` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readRecipeKey :: String -> Maybe RecipeID
readRecipeKey = Database.CDBI.ER.readDatabaseKey "Recipe" RecipeID

--- Gets all `Recipe` entities.
queryAllRecipes :: Database.CDBI.Connection.DBAction [Recipe]
queryAllRecipes = Database.CDBI.ER.getAllEntries recipe_CDBI_Description

--- Gets all `Recipe` entities satisfying a given predicate.
queryCondRecipe
  :: (Recipe -> Bool) -> Database.CDBI.Connection.DBAction [Recipe]
queryCondRecipe = Database.CDBI.ER.getCondEntries recipe_CDBI_Description

--- Gets a `Recipe` entry by a given key.
getRecipe :: RecipeID -> Database.CDBI.Connection.DBAction Recipe
getRecipe =
  Database.CDBI.ER.getEntryWithKey recipe_CDBI_Description recipeColumnKey
   recipeID

--- Inserts a new `Recipe` entity.
newRecipe :: String -> String -> Database.CDBI.Connection.DBAction Recipe
newRecipe name_p reference_p =
  Database.CDBI.ER.insertNewEntry recipe_CDBI_Description setRecipeKey RecipeID
   (Recipe (RecipeID 0) name_p reference_p)

--- Deletes an existing `Recipe` entry by its key.
deleteRecipe :: Recipe -> Database.CDBI.Connection.DBAction ()
deleteRecipe =
  Database.CDBI.ER.deleteEntry recipe_CDBI_Description recipeColumnKey
   (recipeID . recipeKey)

--- Updates an existing `Recipe` entry by its key.
updateRecipe :: Recipe -> Database.CDBI.Connection.DBAction ()
updateRecipe = Database.CDBI.ER.updateEntry recipe_CDBI_Description

--- The ER description of the `RecipeDescription` entity.
recipeDescription_CDBI_Description
  :: Database.CDBI.Description.EntityDescription RecipeDescription
recipeDescription_CDBI_Description =
  Database.CDBI.Description.ED "RecipeDescription"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeInt]
   (\(RecipeDescription
       (RecipeDescriptionID key)
       servings
       ingredients
       directions
       prepTime
       cookTime
       (RecipeID recipeRecDescKey)) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString servings
     ,Database.CDBI.Connection.SQLString ingredients
     ,Database.CDBI.Connection.SQLString directions
     ,Database.CDBI.Description.sqlString prepTime
     ,Database.CDBI.Description.sqlString cookTime
     ,Database.CDBI.Connection.SQLInt recipeRecDescKey])
   (\(RecipeDescription
       _
       servings
       ingredients
       directions
       prepTime
       cookTime
       (RecipeID recipeRecDescKey)) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString servings
     ,Database.CDBI.Connection.SQLString ingredients
     ,Database.CDBI.Connection.SQLString directions
     ,Database.CDBI.Description.sqlString prepTime
     ,Database.CDBI.Description.sqlString cookTime
     ,Database.CDBI.Connection.SQLInt recipeRecDescKey])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString servings
     ,Database.CDBI.Connection.SQLString ingredients
     ,Database.CDBI.Connection.SQLString directions
     ,prepTime
     ,cookTime
     ,Database.CDBI.Connection.SQLInt recipeRecDescKey] ->
     RecipeDescription (RecipeDescriptionID key) servings ingredients directions
      (Database.CDBI.Description.fromStringOrNull prepTime)
      (Database.CDBI.Description.fromStringOrNull cookTime)
      (RecipeID recipeRecDescKey))

--- The database table of the `RecipeDescription` entity.
recipeDescriptionTable :: Database.CDBI.Description.Table
recipeDescriptionTable = "RecipeDescription"

--- The database column `Key` of the `RecipeDescription` entity.
recipeDescriptionColumnKey
  :: Database.CDBI.Description.Column RecipeDescriptionID
recipeDescriptionColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"RecipeDescription\".\"Key\""

--- The database column `Servings` of the `RecipeDescription` entity.
recipeDescriptionColumnServings :: Database.CDBI.Description.Column String
recipeDescriptionColumnServings =
  Database.CDBI.Description.Column "\"Servings\""
   "\"RecipeDescription\".\"Servings\""

--- The database column `Ingredients` of the `RecipeDescription` entity.
recipeDescriptionColumnIngredients :: Database.CDBI.Description.Column String
recipeDescriptionColumnIngredients =
  Database.CDBI.Description.Column "\"Ingredients\""
   "\"RecipeDescription\".\"Ingredients\""

--- The database column `Directions` of the `RecipeDescription` entity.
recipeDescriptionColumnDirections :: Database.CDBI.Description.Column String
recipeDescriptionColumnDirections =
  Database.CDBI.Description.Column "\"Directions\""
   "\"RecipeDescription\".\"Directions\""

--- The database column `PrepTime` of the `RecipeDescription` entity.
recipeDescriptionColumnPrepTime :: Database.CDBI.Description.Column String
recipeDescriptionColumnPrepTime =
  Database.CDBI.Description.Column "\"PrepTime\""
   "\"RecipeDescription\".\"PrepTime\""

--- The database column `CookTime` of the `RecipeDescription` entity.
recipeDescriptionColumnCookTime :: Database.CDBI.Description.Column String
recipeDescriptionColumnCookTime =
  Database.CDBI.Description.Column "\"CookTime\""
   "\"RecipeDescription\".\"CookTime\""

--- The database column `RecipeRecDescKey` of the `RecipeDescription` entity.
recipeDescriptionColumnRecipeRecDescKey
  :: Database.CDBI.Description.Column RecipeID
recipeDescriptionColumnRecipeRecDescKey =
  Database.CDBI.Description.Column "\"RecipeRecDescKey\""
   "\"RecipeDescription\".\"RecipeRecDescKey\""

--- The description of the database column `Key` of the `RecipeDescription` entity.
recipeDescriptionKeyColDesc
  :: Database.CDBI.Description.ColumnDescription RecipeDescriptionID
recipeDescriptionKeyColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(RecipeDescriptionID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> RecipeDescriptionID key)

--- The description of the database column `Servings` of the `RecipeDescription` entity.
recipeDescriptionServingsColDesc
  :: Database.CDBI.Description.ColumnDescription String
recipeDescriptionServingsColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"Servings\""
   Database.CDBI.Connection.SQLTypeString
   (\servings -> Database.CDBI.Connection.SQLString servings)
   (\(Database.CDBI.Connection.SQLString servings) -> servings)

--- The description of the database column `Ingredients` of the `RecipeDescription` entity.
recipeDescriptionIngredientsColDesc
  :: Database.CDBI.Description.ColumnDescription String
recipeDescriptionIngredientsColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"Ingredients\""
   Database.CDBI.Connection.SQLTypeString
   (\ingredients -> Database.CDBI.Connection.SQLString ingredients)
   (\(Database.CDBI.Connection.SQLString ingredients) -> ingredients)

--- The description of the database column `Directions` of the `RecipeDescription` entity.
recipeDescriptionDirectionsColDesc
  :: Database.CDBI.Description.ColumnDescription String
recipeDescriptionDirectionsColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"Directions\""
   Database.CDBI.Connection.SQLTypeString
   (\directions -> Database.CDBI.Connection.SQLString directions)
   (\(Database.CDBI.Connection.SQLString directions) -> directions)

--- The description of the database column `PrepTime` of the `RecipeDescription` entity.
recipeDescriptionPrepTimeColDesc
  :: Database.CDBI.Description.ColumnDescription String
recipeDescriptionPrepTimeColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"PrepTime\""
   Database.CDBI.Connection.SQLTypeString
   (\prepTime -> Database.CDBI.Description.sqlString prepTime)
   (\prepTime -> Database.CDBI.Description.fromStringOrNull prepTime)

--- The description of the database column `CookTime` of the `RecipeDescription` entity.
recipeDescriptionCookTimeColDesc
  :: Database.CDBI.Description.ColumnDescription String
recipeDescriptionCookTimeColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"CookTime\""
   Database.CDBI.Connection.SQLTypeString
   (\cookTime -> Database.CDBI.Description.sqlString cookTime)
   (\cookTime -> Database.CDBI.Description.fromStringOrNull cookTime)

--- The description of the database column `RecipeRecDescKey` of the `RecipeDescription` entity.
recipeDescriptionRecipeRecDescKeyColDesc
  :: Database.CDBI.Description.ColumnDescription RecipeID
recipeDescriptionRecipeRecDescKeyColDesc =
  Database.CDBI.Description.ColDesc "\"RecipeDescription\".\"RecipeRecDescKey\""
   Database.CDBI.Connection.SQLTypeInt
   (\(RecipeID recipeRecDescKey) ->
     Database.CDBI.Connection.SQLInt recipeRecDescKey)
   (\(Database.CDBI.Connection.SQLInt recipeRecDescKey) ->
     RecipeID recipeRecDescKey)

--- Gets the attribute `Key` of the `RecipeDescription` entity.
recipeDescriptionKey :: RecipeDescription -> RecipeDescriptionID
recipeDescriptionKey (RecipeDescription a _ _ _ _ _ _) = a

--- Gets the attribute `Servings` of the `RecipeDescription` entity.
recipeDescriptionServings :: RecipeDescription -> String
recipeDescriptionServings (RecipeDescription _ a _ _ _ _ _) = a

--- Gets the attribute `Ingredients` of the `RecipeDescription` entity.
recipeDescriptionIngredients :: RecipeDescription -> String
recipeDescriptionIngredients (RecipeDescription _ _ a _ _ _ _) = a

--- Gets the attribute `Directions` of the `RecipeDescription` entity.
recipeDescriptionDirections :: RecipeDescription -> String
recipeDescriptionDirections (RecipeDescription _ _ _ a _ _ _) = a

--- Gets the attribute `PrepTime` of the `RecipeDescription` entity.
recipeDescriptionPrepTime :: RecipeDescription -> String
recipeDescriptionPrepTime (RecipeDescription _ _ _ _ a _ _) = a

--- Gets the attribute `CookTime` of the `RecipeDescription` entity.
recipeDescriptionCookTime :: RecipeDescription -> String
recipeDescriptionCookTime (RecipeDescription _ _ _ _ _ a _) = a

--- Gets the attribute `RecipeRecDescKey` of the `RecipeDescription` entity.
recipeDescriptionRecipeRecDescKey :: RecipeDescription -> RecipeID
recipeDescriptionRecipeRecDescKey (RecipeDescription _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `RecipeDescription` entity.
setRecipeDescriptionKey
  :: RecipeDescription -> RecipeDescriptionID -> RecipeDescription
setRecipeDescriptionKey (RecipeDescription _ b6 b5 b4 b3 b2 b1) a =
  RecipeDescription a b6 b5 b4 b3 b2 b1

--- Sets the attribute `Servings` of the `RecipeDescription` entity.
setRecipeDescriptionServings :: RecipeDescription -> String -> RecipeDescription
setRecipeDescriptionServings (RecipeDescription a2 _ b5 b4 b3 b2 b1) a =
  RecipeDescription a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Ingredients` of the `RecipeDescription` entity.
setRecipeDescriptionIngredients
  :: RecipeDescription -> String -> RecipeDescription
setRecipeDescriptionIngredients (RecipeDescription a3 a2 _ b4 b3 b2 b1) a =
  RecipeDescription a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Directions` of the `RecipeDescription` entity.
setRecipeDescriptionDirections
  :: RecipeDescription -> String -> RecipeDescription
setRecipeDescriptionDirections (RecipeDescription a4 a3 a2 _ b3 b2 b1) a =
  RecipeDescription a4 a3 a2 a b3 b2 b1

--- Sets the attribute `PrepTime` of the `RecipeDescription` entity.
setRecipeDescriptionPrepTime :: RecipeDescription -> String -> RecipeDescription
setRecipeDescriptionPrepTime (RecipeDescription a5 a4 a3 a2 _ b2 b1) a =
  RecipeDescription a5 a4 a3 a2 a b2 b1

--- Sets the attribute `CookTime` of the `RecipeDescription` entity.
setRecipeDescriptionCookTime :: RecipeDescription -> String -> RecipeDescription
setRecipeDescriptionCookTime (RecipeDescription a6 a5 a4 a3 a2 _ b1) a =
  RecipeDescription a6 a5 a4 a3 a2 a b1

--- Sets the attribute `RecipeRecDescKey` of the `RecipeDescription` entity.
setRecipeDescriptionRecipeRecDescKey
  :: RecipeDescription -> RecipeID -> RecipeDescription
setRecipeDescriptionRecipeRecDescKey (RecipeDescription a7 a6 a5 a4 a3 a2 _) a =
  RecipeDescription a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `RecipeDescription`.
recipeDescriptionID
  :: RecipeDescriptionID -> Database.CDBI.Criteria.Value RecipeDescriptionID
recipeDescriptionID (RecipeDescriptionID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `RecipeDescription`.
recipeDescriptionKeyToInt :: RecipeDescriptionID -> Int
recipeDescriptionKeyToInt (RecipeDescriptionID key) = key

--- Shows the key of a `RecipeDescription` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showRecipeDescriptionKey :: RecipeDescription -> String
showRecipeDescriptionKey entry =
  Database.CDBI.ER.showDatabaseKey "RecipeDescription" recipeDescriptionKeyToInt
   (recipeDescriptionKey entry)

--- Transforms a string into a key of a `RecipeDescription` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readRecipeDescriptionKey :: String -> Maybe RecipeDescriptionID
readRecipeDescriptionKey =
  Database.CDBI.ER.readDatabaseKey "RecipeDescription" RecipeDescriptionID

--- Gets all `RecipeDescription` entities.
queryAllRecipeDescriptions
  :: Database.CDBI.Connection.DBAction [RecipeDescription]
queryAllRecipeDescriptions =
  Database.CDBI.ER.getAllEntries recipeDescription_CDBI_Description

--- Gets all `RecipeDescription` entities satisfying a given predicate.
queryCondRecipeDescription
  :: (RecipeDescription -> Bool)
  -> Database.CDBI.Connection.DBAction [RecipeDescription]
queryCondRecipeDescription =
  Database.CDBI.ER.getCondEntries recipeDescription_CDBI_Description

--- Gets a `RecipeDescription` entry by a given key.
getRecipeDescription
  :: RecipeDescriptionID -> Database.CDBI.Connection.DBAction RecipeDescription
getRecipeDescription =
  Database.CDBI.ER.getEntryWithKey recipeDescription_CDBI_Description
   recipeDescriptionColumnKey
   recipeDescriptionID

--- Inserts a new `RecipeDescription` entity.
newRecipeDescriptionWithRecipeRecDescKey
  :: String
  -> String
  -> String
  -> String
  -> String -> RecipeID -> Database.CDBI.Connection.DBAction RecipeDescription
newRecipeDescriptionWithRecipeRecDescKey
    servings_p
    ingredients_p
    directions_p
    prepTime_p
    cookTime_p
    recipeRecDescKey_p =
  Database.CDBI.ER.insertNewEntry recipeDescription_CDBI_Description
   setRecipeDescriptionKey
   RecipeDescriptionID
   (RecipeDescription (RecipeDescriptionID 0) servings_p ingredients_p
     directions_p
     prepTime_p
     cookTime_p
     recipeRecDescKey_p)

--- Deletes an existing `RecipeDescription` entry by its key.
deleteRecipeDescription
  :: RecipeDescription -> Database.CDBI.Connection.DBAction ()
deleteRecipeDescription =
  Database.CDBI.ER.deleteEntry recipeDescription_CDBI_Description
   recipeDescriptionColumnKey
   (recipeDescriptionID . recipeDescriptionKey)

--- Updates an existing `RecipeDescription` entry by its key.
updateRecipeDescription
  :: RecipeDescription -> Database.CDBI.Connection.DBAction ()
updateRecipeDescription =
  Database.CDBI.ER.updateEntry recipeDescription_CDBI_Description

--- Saves complete database as term files into an existing directory
--- provided as a parameter.
saveDBTo :: String -> IO ()
saveDBTo dir =
  do Database.CDBI.ER.saveDBTerms tagging_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms recipeCategory_CDBI_Description sqliteDBFile
      dir
     Database.CDBI.ER.saveDBTerms category_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms keyword_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms recipe_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.saveDBTerms recipeDescription_CDBI_Description
      sqliteDBFile
      dir

--- Restores complete database from term files which are stored
--- in a directory provided as a parameter.
restoreDBFrom :: String -> IO ()
restoreDBFrom dir =
  do Database.CDBI.ER.restoreDBTerms tagging_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms recipeCategory_CDBI_Description
      sqliteDBFile
      dir
     Database.CDBI.ER.restoreDBTerms category_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms keyword_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms recipe_CDBI_Description sqliteDBFile dir
     Database.CDBI.ER.restoreDBTerms recipeDescription_CDBI_Description
      sqliteDBFile
      dir

--- Runs a DB action (typically a query).
runQ :: Database.CDBI.Connection.DBAction a -> IO a
runQ = Database.CDBI.ER.runQueryOnDB sqliteDBFile

--- Runs a DB action as a transaction.
runT
  :: Database.CDBI.Connection.DBAction a
  -> IO (Database.CDBI.Connection.SQLResult a)
runT = Database.CDBI.ER.runTransactionOnDB sqliteDBFile

--- Runs a DB action as a transaction. Emits an error in case of failure.
runJustT :: Database.CDBI.Connection.DBAction a -> IO a
runJustT = Database.CDBI.ER.runJustTransactionOnDB sqliteDBFile
