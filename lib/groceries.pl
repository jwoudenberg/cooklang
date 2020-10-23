:- module(groceries, [mealplan/1, with/2, without/2, search_recipe/2, grocery_list/2]).

% Example usage:
%
%     ?- mealplan([Mo, Tu, We, Th, Fr, Sa, Su]).
mealplan(Recipes) :-
  findall(R, favorite(R), Favorites),
  sublist(Recipes, Favorites),
  maplist(recipe, Recipes).

sublist([], _).
sublist([X|Xs], [Y|Ys]) :-
  (X = Y, sublist(Xs, Ys));
  sublist([X|Xs], Ys).

recipe(Query) :-
  find_recipe(Query, _).

% For use in combination with mealplan.
%
%     meat(pork)
%     meat(beef)
%     meat(chicken)
%
%     ?- mealplan([Mo, Tu]), without(Tu, meat).
without(Query, Goal) :-
  find_recipe(Query, Name),
  ingredients(Name, Ingredients),
  maplist(get_ingredient, Ingredients, IngredientNames),
  maplist(ingredient_without(Goal), IngredientNames).

ingredient_without(Goal, I) :-
  findall(V, call(Goal, V), Is),
  maplist(not_sub_string(I), Is).

% For use in combination with mealplan.
%
%     meat(pork)
%     meat(beef)
%     meat(chicken)
%
%     ?- mealplan([Mo, Tu]), with(Tu, meat).
with(Query, Goal) :-
  find_recipe(Query, Name),
  ingredients(Name, Ingredients),
  maplist(get_ingredient, Ingredients, IngredientNames),
  include(ingredient_with(Goal), IngredientNames, [_ |_]).

ingredient_with(Goal, I) :-
  findall(V, call(Goal, V), Is),
  include(yes_sub_string(I), Is, [_ | _]).

yes_sub_string(I, X) :-
  sub_string(I, _, _, _, X).

not_sub_string(I, X) :-
  \+ sub_string(I, _, _, _, X).

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
  maplist(portioned_ingredients, Recipes, NestedGroceries),
  append(NestedGroceries, DupedGroceries),
  dedupe(DupedGroceries, Groceries).

parse_input(recipe(Query, Portions), recipe(Name, Portions)) :-
  find_recipe(Query, Name).
parse_input(Query, recipe(Name, DefaultPortions)) :-
  atomic(Query),
  find_recipe(Query, Name),
  portions(Name, DefaultPortions).

portioned_ingredients(recipe(Name, Portions), Ingredients) :-
  portions(Name, RecipePortions),
  ingredients(Name, IngredientsForDefaultPortions),
  Factor is Portions/RecipePortions,
  maplist(multiply_quantity(Factor), IngredientsForDefaultPortions, Ingredients).

ingredients(Name, Ingredients) :-
  findall(Ingredient, contains(Name, Ingredient), Ingredients).

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

with_ingredient(P, I) :-
  get_ingredient(I, Ingredient),
  call(P, Ingredient).

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
