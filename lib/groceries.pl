:- module(groceries, [mealplan/1, with/2, without/2, search_recipe/2, print_grocery_list/0]).

% Example usage:
%
%     ?- mealplan([Mo, Tu, We, Th, Fr, Sa, Su]).
mealplan(Plan) :-
  get_time(Now),
  findall(R, favorite(R), Favorites),
  maplist(find_recipe, Favorites, Recipes),
  recency_scores(Now, Recipes, ScoresByRecipe),
  mealplan_helper(ScoresByRecipe, Plan).

mealplan_helper(_, []).
mealplan_helper(ScoresByRecipe, [First|Rest]) :-
  next_meal(ScoresByRecipe, ScoresByRecipeAfter, First),
  mealplan_helper(ScoresByRecipeAfter, Rest).

% Get a list of your favorites in decreasing order of what you haven't eaten
% in a while.
next_meal(ScoresByRecipe, ScoresByRecipeAfter, Recipe) :-
  assoc_to_keys(ScoresByRecipe, Recipes),
  predsort(order_recipes(ScoresByRecipe), Recipes, OrderedRecipes),
  member(Recipe, OrderedRecipes),
  get_assoc(Recipe, ScoresByRecipe, Score, ScoresByRecipeAfter, ScoreAfter),
  ScoreAfter is Score + 1.

order_recipes(ScoresByRecipe, Order, Recipe1, Recipe2) :-
  get_assoc(Recipe1, ScoresByRecipe, Score1),
  get_assoc(Recipe2, ScoresByRecipe, Score2),
  compare(Order, [Score1, Recipe1], [Score2, Recipe2]).

recency_scores(Now, Recipes, ScoresByRecipe) :-
  maplist(recency_score(Now), Recipes, Scores),
  pairs_keys_values(RecipeScorePairs, Recipes, Scores),
  list_to_assoc(RecipeScorePairs, ScoresByRecipe).

recency_score(Now, Recipe, Score) :-
  findall(D, planned_full_name(D, Recipe), Dates),
  maplist(recency_score_for_date(Now), Dates, Scores),
  sumlist(Scores, Score).

planned_full_name(Date, Full) :-
  planned(Date, Short, _),
  find_recipe(Short, Full).

% Score each date based on how far away it is from now. The current time gets a
% score of 1. That score is halved for each week into the future or past.
%
%   Two weeks from now: 0.25
%   One week from now:  0.5
%   Now:                1
%   One week ago:       0.5
%   Two weeks ago:      0.25
recency_score_for_date(Now, Date, Score) :-
  date_time_stamp(Date, Timestamp),
  Score is 1 / (1 + (abs(Now - Timestamp) / (3600 * 24 * 7))).

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
  once(search_recipe(Query, Name)) ->
  true;
  (
    format(atom(Exception), "Can't find recipe '~w'", [Query]),
    throw(Exception)
  ).

:- begin_tests(find_recipe).

test("single keyword") :-
  find_recipe("Spaghetti", "Spaghetti Bolognese").

test("multiple keywords") :-
  find_recipe("broccoli curry", "Egg curry with broccoli").

test("capitalization insensitive") :-
  find_recipe("egG", "Egg curry with broccoli").

:- end_tests(find_recipe).

contains_keyword(Full, Keyword) :-
  string_lower(Full, FullLower),
  string_lower(Keyword, KeywordLower),
  sub_string(FullLower, _, _, _, KeywordLower).

print_grocery_list :-
  open('groceries.txt', append, Stream),
  grocery_list_from(FromDate),
  grocery_list(FromDate, Groceries),
  maplist(print_grocery(Stream), Groceries),
  close(Stream).

print_grocery(Stream, ingredient(Name)) :-
  format(Stream, "~w~n", [Name]).
print_grocery(Stream, ingredient(Quantity, Name)) :-
  format(Stream, "~w, ~w~n", [Name, Quantity]).
print_grocery(Stream, ingredient(Quantity, Unit, Name)) :-
  format(Stream, "~w, ~w ~w~n", [Name, Quantity, Unit]).

grocery_list(FromDate, Groceries) :-
  findall(Recipe, planned_from(FromDate, Recipe), Queries),
  maplist(parse_input, Queries, Recipes),
  maplist(portioned_ingredients, Recipes, NestedGroceries),
  append(NestedGroceries, DupedGroceries),
  dedupe(DupedGroceries, Groceries).

planned_from(FromDate, recipe(Query, Portions)) :-
  planned(PlannedDate, Query, Portions),
  date_time_stamp(PlannedDate, PlannedTimestamp),
  date_time_stamp(FromDate, FromTimestamp),
  PlannedTimestamp >= FromTimestamp.

parse_input(recipe(RecipeQuery, Portions),  recipe(Name, Portions)) :-
  find_recipe(RecipeQuery, Name).

portioned_ingredients(recipe(Name, Portions), Ingredients) :-
  portions(Name, RecipePortions),
  ingredients(Name, IngredientsForDefaultPortions),
  Factor is Portions/RecipePortions,
  maplist(multiply_quantity(Factor), IngredientsForDefaultPortions, Ingredients).

ingredients(Name, Ingredients) :-
  findall(Ingredient, contains(Name, Ingredient), Ingredients).

:- begin_tests(ingredients).

test(bolognese) :-
  ingredients("Spaghetti Bolognese", [
    ingredient(500, g, "minced beef"),
    ingredient(100, g, "ham"),
    ingredient(1, "onion"),
    ingredient(1, toe, "garlic"),
    ingredient(2, cans, "peeled tomatoes"),
    ingredient("small can of tomato puree"),
    ingredient(1, dl, "red whine"),
    ingredient(1, "bay leaf"),
    ingredient("oregano"),
    ingredient("salt"),
    ingredient("grated cheese"),
    ingredient(500, g, "spaghetti")
  ]).

:- end_tests(ingredients).

dedupe(Duped, Deduped) :-
  empty_assoc(Init),
  foldl(add_ingredient,Duped,Init,DedupedAssoc),
  assoc_to_values(DedupedAssoc, Deduped).

:- begin_tests(dedupe).

test("empty list") :-
  dedupe([], []).

test("different ingredients") :-
  dedupe(
    [ingredient(5, g, sausage), ingredient(100, g, sugar)],
    [ingredient(5, g, sausage), ingredient(100, g, sugar)]
  ).

test("same units") :-
  dedupe(
    [ingredient(5, g, sugar), ingredient(100, g, sugar)],
    [ingredient(105, g, sugar)]
  ).

test("convertable units") :-
  dedupe(
    [ingredient(5, kg, sugar), ingredient(100, g, sugar)],
    [ingredient(5.1, kg, sugar)]
  ).

test("unconvertable units") :-
  dedupe(
    [ingredient(5, kg, sugar), ingredient(1, blikje, sugar)],
    [ingredient("5 kg + 1 blikje", sugar)]
  ).

test("unit and non-unit") :-
  dedupe(
    [ingredient(5, kg, sugar), ingredient(1, sugar)],
    [ingredient("5 kg + 1", sugar)]
  ).

:- end_tests(dedupe).

add_ingredient(Ingredient,AssocWithout,AssocWith) :-
  get_ingredient(Ingredient,Key),
  (
    get_assoc(Key, AssocWithout, Accum, AssocWith, Sum) ->
    (
      add_ingredients(Accum, Ingredient, Sum)
    );
    (
      put_assoc(Key, AssocWithout, Ingredient, AssocWith)
    )
  ).

with_ingredient(P, I) :-
  get_ingredient(I, Ingredient),
  call(P, Ingredient).

get_ingredient(ingredient(_, _, Ingredient), Ingredient).
get_ingredient(ingredient(_, Ingredient   ), Ingredient).
get_ingredient(ingredient(Ingredient      ), Ingredient).

add_ingredients(I1, I2, IR) :-
  once(add_ingredients_helper(I1, I2, IR)).

add_ingredients_helper(ingredient(I      ), ingredient(I      ), ingredient(I      )).
add_ingredients_helper(ingredient(X, I   ), ingredient(Y, I   ), ingredient(S, I   )) :-
  number(X),
  number(Y),
  S is X+Y.
add_ingredients_helper(ingredient(X, I   ), ingredient(Y, I   ), ingredient(S, I   )) :-
  append(X,Y,S).
add_ingredients_helper(ingredient(X, U, I), ingredient(Y, U, I), ingredient(S, U, I)) :- S is X+Y.
add_ingredients_helper(ingredient(X, U, I), ingredient(Y, V, I), ingredient(S, U, I)) :-
  convert(quantity(Y, V), quantity(Y2, U)),
  S is X+Y2.
add_ingredients_helper(ingredient(X, U, I), ingredient(Y, V, I), ingredient(S, V, I)) :-
  convert(quantity(X, U), quantity(X2, V)),
  S is X2+Y.
add_ingredients_helper(ingredient(X, U, I), ingredient(Y, V, I), ingredient(S, W, I)) :-
  convert(quantity(X, U), quantity(X2, W)),
  convert(quantity(Y, V), quantity(Y2, W)),
  S is X2+Y2.
add_ingredients_helper(I1, I2, ingredient(QR, I)) :-
  get_ingredient(I1, I),
  quantity_unit_string(I1, Q1),
  quantity_unit_string(I2, Q2),
  atomics_to_string([Q1, Q2], " + ", QR).

quantity_unit_string(ingredient(_), "").
quantity_unit_string(ingredient(Unit, _), Unit).
quantity_unit_string(ingredient(Quantity, Unit, _), String) :-
  atomics_to_string([Quantity, Unit], " ", String).

:- begin_tests(add_ingredients).

test("without quantities") :-
  add_ingredients(
    ingredient(sugar),
    ingredient(sugar),
    ingredient(sugar)
  ).

test("without units") :-
  add_ingredients(
    ingredient(2, cars),
    ingredient(3, cars),
    ingredient(5, cars)
  ).

test("with units 1") :-
  add_ingredients(
    ingredient(100, g, sugar),
    ingredient(5, kg, sugar),
    ingredient(5.1, kg, sugar)
  ).

test("with units 2") :-
  add_ingredients(
    ingredient(5, kg, sugar),
    ingredient(100, g, sugar),
    ingredient(5.1, kg, sugar)
  ).

test("with 2 non-standard units") :-
  add_ingredients(
    ingredient(1, el, sugar),
    ingredient(2, tl, sugar),
    ingredient(25, ml, sugar)
  ).

:- end_tests(add_ingredients).

multiply_quantity(_,      ingredient(I      ), ingredient(I      )).
multiply_quantity(Factor, ingredient(Q, I   ), ingredient(M, I   )) :- M is Q*Factor.
multiply_quantity(Factor, ingredient(Q, U, I), ingredient(M, U, I)) :- M is Q*Factor.

conversion(quantity(X, g), quantity(Y, kg)) :- Y is X/1000.
conversion(quantity(X, el), quantity(Y, ml)) :- Y is X*15.
conversion(quantity(X, tl), quantity(Y, ml)) :- Y is X*5.
conversion(quantity(X, kop), quantity(Y, ml)) :- Y is X*200.
conversion(quantity(X, Unit), quantity(Y, Base)) :-
  atom_concat("d", Base, Unit) -> Y is X / 10;
  atom_concat("c", Base, Unit) -> Y is X / 100;
  atom_concat("m", Base, Unit) -> Y is X / 1000.

% Able to perform a chain of multiple conversions.
convert(quantity(X, U), quantity(Y, V)) :-
  conversion(quantity(X, U), quantity(Y, V)) ->
  true;
  (
    conversion(quantity(X, U), quantity(Z, W)),
    convert(quantity(Z, W), quantity(Y, V))
  ) ->
  true.

:- begin_tests(convert).

test("direct conversion") :-
  convert(quantity(100, g), quantity(0.1, kg)).

test("SI conversion") :-
  convert(quantity(100, cg), quantity(1, g)).

test("conversion in multiple steps") :-
  convert(quantity(1000, dg), quantity(0.1, kg)).

:- end_tests(convert).
