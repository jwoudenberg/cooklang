:- module(groceries, [parse_recipe/2]).
:- asserta(user:file_search_path(library, 'packs/markdown/prolog')).
:- style_check(-discontiguous).
:- use_module(library(md/md_parse)).

% Bolognese

recipe("Spaghetti bolognese a la mama", 4).
contains("Spaghetti bolognese a la mama", "1 blikje tomatenblokjes").
contains("Spaghetti bolognese a la mama", "100 g spaghetti").
contains("Spaghetti bolognese a la mama", "400g gehakt").
contains("Spaghetti bolognese a la mama", "2kg kaas").

% Aubergineschotel

recipe("Aubergineschotel met kaas", 2).
contains("Aubergineschotel met kaas", "1.5 aubergines").
contains("Aubergineschotel met kaas", "200g kaas").
contains("Aubergineschotel met kaas", "1 blikje tomatenblokjes").

% Helpers

parse_recipe(Stream, Path) :-
  format("Parsing: ~w~n", Path),
  md_parse_file(Path, Blocks),
  parse_name(Name, Blocks),
  parse_portions(Portions, Blocks),
  parse_ingredients(Ingredients, Blocks),
  write_fact(Stream, portions(Name, Portions)),
  maplist(write_ingredient(Stream, Name),Ingredients),
  format(Stream, "~n", []).

write_ingredient(Stream, Name, Ingredient) :-
  write_fact(Stream, contains(Name, Ingredient)).

write_fact(Stream, Term) :-
  format(Stream, '~q.~n', Term).

parse_name(Name, Blocks) :-
  member(h1(Name), Blocks).

parse_ingredients(Ingredients, Blocks) :-
  member(ul(LiIngredients), Blocks),
  maplist(without_li, LiIngredients, IngredientStrings),
  maplist(parse_ingredient, IngredientStrings, Ingredients).

parse_portions(Portions, Blocks) :-
  member(p([\[PortionsLine]]), Blocks),
  sub_string(PortionsLine, _, _, _, PortionsWord),
  number_string(Portions, PortionsWord).
parse_portions(1, _).

without_li(li([\[X]]), X).
without_li(li([X]), X).
without_li(li(X), X).

full_name(Part, Full) :-
  recipe(Full, _),
  string_lower(Full, FullLower),
  sub_string(FullLower, _, _, _, Part).

grocery_list(Recipes, Groceries) :-
  maplist(ingredients, Recipes, NestedGroceries),
  append(NestedGroceries, DupedGroceries),
  dedupe(DupedGroceries, Groceries).

ingredients(portions(Recipe, Portions), Ingredients) :-
  full_name(Recipe, FullName),
  recipe(FullName, RecipePortions),
  findall(Ingredient, contains(FullName, Ingredient), IngredientStrings),
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

parse_ingredient(String, I) :-
  (
    split_string(String, " ", "", [QuantityWord, UnitWord | IngredientWords]),
    atom_string(Unit, UnitWord),
    unit(Unit)
  ) ->
  (
    number_string(Quantity, QuantityWord),
    atomics_to_string(IngredientWords, ' ', Ingredient),
    I = ingredient(Quantity, Unit, Ingredient)
  );
  (
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    quantity_with_unit(QuantityWord, Quantity, Unit)
  ) ->
  (
    atomics_to_string(IngredientWords, ' ', Ingredient),
    I = ingredient(Quantity, Unit, Ingredient)
  );
  (
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    number_string(Quantity, QuantityWord)
  ) ->
  (
    atomics_to_string(IngredientWords, ' ', Ingredient),
    I = ingredient(Quantity, Ingredient)
  );
  (
    I = ingredient(String)
  ).

quantity_with_unit(String, Quantity, Unit) :-
  string_concat(QuantityString, UnitString, String),
  atom_string(Unit, UnitString),
  unit(Unit),
  number_string(Quantity, QuantityString).

unit(g).
unit(kg).

convert(quantity(X, gram), quantity(X, g)).
convert(quantity(X, g), quantity(Y, kg)) :- Y is X/1000.
