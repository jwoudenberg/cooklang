:- module(groceries, [search_recipe/2, grocery_list/2]).

search_recipe(Query, Name) :-
  portions(Name, _),
  split_string(Query, " ", "", Keywords),
  maplist(contains_keyword(Name), Keywords).

find_recipe(Query, Name) :-
  once(search_recipe(Query, Name)).

contains_keyword(Full, Keyword) :-
  string_lower(Full, FullLower),
  string_lower(Keyword, KeywordLower),
  sub_string(FullLower, _, _, _, KeywordLower).

grocery_list(Queries, Groceries) :-
  maplist(parse_input, Queries, Recipes),
  maplist(ingredients, Recipes, NestedGroceries),
  append(NestedGroceries, DupedGroceries),
  dedupe(DupedGroceries, Groceries).

parse_input(recipe(Query, Portions), recipe(Name, Portions)) :-
  find_recipe(Query, Name).
parse_input(Query, recipe(Name, DefaultPortions)) :-
  atomic(Query),
  find_recipe(Query, Name),
  portions(Name, DefaultPortions).

ingredients(recipe(Name, Portions), Ingredients) :-
  portions(Name, RecipePortions),
  findall(Ingredient, contains(Name, Ingredient), IngredientsForDefaultPortions),
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
get_ingredient(ingredient(_, Ingredient   ), Ingredient).
get_ingredient(ingredient(Ingredient      ), Ingredient).

add_ingredients(ingredient(I      ), ingredient(I      ), ingredient(I      )).
add_ingredients(ingredient(X, I   ), ingredient(Y, I   ), ingredient(S, I   )) :- S is X+Y.
add_ingredients(ingredient(X, U, I), ingredient(Y, U, I), ingredient(S, U, I)) :- S is X+Y.
add_ingredients(ingredient(X, U, I), ingredient(Y, V, I), ingredient(S, U, I)) :-
  convert(quantity(Y, V), quantity(Y2, U)),
  S is X+Y2.
add_ingredients(ingredient(X, U, I), ingredient(Y, V, I), ingredient(S, V, I)) :-
  convert(quantity(X, U), quantity(X2, V)),
  S is X2+Y.

multiply_quantity(_,      ingredient(I      ), ingredient(I      )).
multiply_quantity(Factor, ingredient(Q, I   ), ingredient(M, I   )) :- M is Q*Factor.
multiply_quantity(Factor, ingredient(Q, U, I), ingredient(M, U, I)) :- M is Q*Factor.

convert(quantity(X, gram), quantity(X, g )).
convert(quantity(X, g   ), quantity(Y, kg)) :- Y is X/1000.
