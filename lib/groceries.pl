:- module(groceries, [grocery_list/2]).
:- use_module(recipes).

full_name(Part, Full) :-
  portions(Full, _),
  string_lower(Full, FullLower),
  sub_string(FullLower, _, _, _, Part).

grocery_list(Recipes, Groceries) :-
  maplist(ingredients, Recipes, NestedGroceries),
  append(NestedGroceries, DupedGroceries),
  dedupe(DupedGroceries, Groceries).

ingredients(portions(Recipe, Portions), Ingredients) :-
  full_name(Recipe, FullName),
  portions(FullName, RecipePortions),
  findall(Ingredient, contains(FullName, Ingredient), IngredientsForDefaultPortions),
  Factor is Portions/RecipePortions,
  maplist(multiply_quantity(Factor), IngredientsForDefaultPortions, Ingredients).

dedupe(Duped, Deduped) :-
  empty_assoc(Init),
  foldl(add_ingredient,Duped,Init,DedupedAssoc),
  assoc_to_values(DedupedAssoc, Deduped).

add_ingredient(Ingredient,AssocWithout,AssocWith) :-
  get_ingredient(Ingredient,Key),
  get_assoc(Key, AssocWithout, Accum, AssocWith, Sum),
  add_ingredients(Accum, Ingredient, Sum).
add_ingredient(Ingredient,AssocWithout,AssocWith) :-
  get_ingredient(Ingredient,Key),
  \+ get_assoc(Key, AssocWithout, _),
  put_assoc(Key, AssocWithout, Ingredient, AssocWith).

get_ingredient(ingredient(_, _, Ingredient), Ingredient).
get_ingredient(ingredient(_, Ingredient), Ingredient).
get_ingredient(ingredient(Ingredient), Ingredient).

add_ingredients(ingredient(I), ingredient(I), ingredient(I)).
add_ingredients(ingredient(X, I), ingredient(Y, I), ingredient(Sum, I)) :-
  Sum is X+Y.
add_ingredients(ingredient(X, U, I), ingredient(Y, U, I), ingredient(Sum, U, I)) :-
  Sum is X+Y.
add_ingredients(ingredient(X, UX, I), ingredient(Y, UY, I), ingredient(Sum, UX, I)) :-
  convert(quantity(Y, UY), quantity(Y2, UX)),
  Sum is X+Y2.
add_ingredients(ingredient(X, UX, I), ingredient(Y, UY, I), ingredient(Sum, UY, I)) :-
  convert(quantity(X, UX), quantity(X2, UY)),
  Sum is X2+Y.

multiply_quantity(_, ingredient(Ingredient), ingredient(Ingredient)).
multiply_quantity(Factor, ingredient(Quantity, Ingredient), ingredient(MultipliedQuantity, Ingredient)) :-
  MultipliedQuantity is Quantity*Factor.
multiply_quantity(Factor, ingredient(Quantity, Unit, Ingredient), ingredient(MultipliedQuantity, Unit, Ingredient)) :-
  MultipliedQuantity is Quantity*Factor.

convert(quantity(X, gram), quantity(X, g)).
convert(quantity(X, g), quantity(Y, kg)) :- Y is X/1000.
