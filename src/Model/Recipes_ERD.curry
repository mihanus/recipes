-- ERD for recipe management

import Database.ERD

recipesERD :: ERD
recipesERD =
 ERD "Recipes" 
  [Entity "Category"
    [Attribute "Name"     (StringDom Nothing) NoKey False,
     Attribute "Position" (IntDom    Nothing) NoKey False],
   Entity "Keyword"  [Attribute "Name" (StringDom Nothing) Unique False],
   Entity "Recipe"
    [Attribute "Name"        (StringDom Nothing) NoKey False,
     Attribute "Reference"   (StringDom Nothing) NoKey True],
   Entity "RecipeDescription" 
    [Attribute "Servings"    (StringDom Nothing) NoKey False,
     Attribute "Ingredients" (StringDom Nothing) NoKey False, 
     Attribute "Directions"  (StringDom Nothing) NoKey False, 
     Attribute "PrepTime"    (StringDom Nothing) NoKey True, 
     Attribute "CookTime"    (StringDom Nothing) NoKey True
    ]
  ]
  [Relationship "ParentCategory"
    [REnd "Category" "catInCategory"   (Between 0 (Max 1)),
     REnd "Category" "catWithCategory" (Between 0 Infinite)],
   Relationship "RecipeCategory"
    [REnd "Category" "recInCategory" (Between 0 Infinite),
     REnd "Recipe"   "catWithRecipe" (Between 0 Infinite)],
   Relationship "Tagging"
    [REnd "Recipe"  "keywordsOf"     (Between 0 Infinite),
     REnd "Keyword" "recWithKeyword" (Between 0 Infinite)],
   Relationship "RecDesc"
    [REnd "Recipe"            "descOf"   (Exactly 1),
     REnd "RecipeDescription" "withDesc" (Between 0 (Max 1))]
   ]
