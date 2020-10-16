:-style_check(-discontiguous).

% Bolognese

recipe("Spaghetti bolognese a la mama", 4, bolognese).
contains(bolognese, "1 blikje tomatenblokjes").
contains(bolognese, "100 g spaghetti").
contains(bolognese, "400g gehakt").
contains(bolognese, "2kg kaas").

% Aubergineschotel

recipe("Aubergineschotel met kaas", 2, aubergineschotel).
contains(aubergineschotel, "1.5 aubergines").
contains(aubergineschotel, "200g kaas").
contains(aubergineschotel, "1 blikje tomatenblokjes").

% Helpers

full_name(Part, Full) :-
  recipe(Full, _, _),
  string_lower(Full, FullLower),
  first_sub_string(FullLower, Part).

first_sub_string(Full, Part) :-
  sub_string(Full, _, _, _, Part),
  !.

grocery_list(Recipes, Groceries) :-
  maplist(ingredients, Recipes, NestedGroceries),
  append(NestedGroceries, DupedGroceries),
  dedupe(DupedGroceries, Groceries).

ingredients(portions(Recipe, Portions), Ingredients) :-
  full_name(Recipe, FullName),
  recipe(FullName, RecipePortions, ShortHand),
  findall(Ingredient, contains(ShortHand, Ingredient), IngredientStrings),
  maplist(parse_ingredient, IngredientStrings, IngredientsForDefaultPortions),
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

multiply_quantity(Factor, ingredient(Quantity, Ingredient), ingredient(MultipliedQuantity, Ingredient)) :-
  MultipliedQuantity is Quantity*Factor.
multiply_quantity(Factor, ingredient(Quantity, Unit, Ingredient), ingredient(MultipliedQuantity, Unit, Ingredient)) :-
  MultipliedQuantity is Quantity*Factor.

parse_ingredient(String, ingredient(Quantity, Unit, Ingredient)) :-
    split_string(String, " ", "", [QuantityWord, UnitWord | IngredientWords]),
    atom_string(Unit, UnitWord),
    unit(Unit),
    !,
    number_string(Quantity, QuantityWord),
    atomics_to_string(IngredientWords, ' ', Ingredient).
parse_ingredient(String, ingredient(Quantity, Unit, Ingredient)) :-
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    quantity_with_unit(QuantityWord, Quantity, Unit),
    atomics_to_string(IngredientWords, ' ', Ingredient).
parse_ingredient(String, ingredient(Quantity, Ingredient)) :-
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    number_string(Quantity, QuantityWord),
    atomics_to_string(IngredientWords, ' ', Ingredient).

quantity_with_unit(String, Quantity, Unit) :-
  string_concat(QuantityString, UnitString, String),
  atom_string(Unit, UnitString),
  unit(Unit),
  number_string(Quantity, QuantityString).

unit(g).
unit(kg).

convert(quantity(X, gram), quantity(X, g)).
convert(quantity(X, g), quantity(Y, kg)) :- Y is X/1000.
