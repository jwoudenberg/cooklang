% Parsing ingredients from markdown recipe files into a knowledge base.

:- module(recipe_parsing, [parse_recipe/1, parse_recipe/2]).
:- asserta(user:file_search_path(library, 'packs/markdown/prolog')).
:- use_module(library(md/md_parse)).

parse_recipe(Path) :-
  open("/dev/stdout", append, Stream),
  parse_recipe(Stream, Path),
  close(Stream).

parse_recipe(Stream, Path) :-
  (
    md_parse_file(Path, Blocks),
    parse_name(Name, Blocks),
    parse_portions(Portions, Blocks),
    parse_ingredients(Ingredients, Blocks)
  ) ->
  (
    write_fact(Stream, portions(Name, Portions)),
    maplist(write_ingredient(Stream, Name), Ingredients),
    format(Stream, "~n", [])
  );
  (
    format("Failed to parse ~w~n", [Path])
  ).

write_ingredient(Stream, Name, Ingredient) :-
  write_fact(Stream, contains(Name, Ingredient)).

write_fact(Stream, Term) :-
  format(Stream, '~q.~n', Term).

parse_name(Name, Blocks) :-
  member(h1(Name), Blocks).

% This rule makes the following assumptions about the recipe markdown:
%
% - The markdown contains a single unordered list.
% - All the items in that list are separate ingredients.
parse_ingredients(Ingredients, Blocks) :-
  member(ul(LiIngredients), Blocks),
  maplist(unwrap, LiIngredients, IngredientStrings),
  maplist(parse_ingredient, IngredientStrings, Ingredients).

parse_portions(Portions, Blocks) :-
  member(p([\[PortionsLine]]), Blocks),
  sub_string(PortionsLine, _, _, _, PortionsWord),
  number_string(Portions, PortionsWord).
parse_portions(1, _).

unwrap(Html, Contents) :-
  Html = li(Wrapped) ->
  unwrap(Wrapped, Contents);
  Html = p(Wrapped) ->
  unwrap(Wrapped, Contents);
  Html = \Wrapped ->
  unwrap(Wrapped, Contents);
  maplist(unwrap, Html, Results) ->
  atomics_to_string(Results, '', Contents);
  Html = Contents.

:- begin_tests(parse_ingredient).

test(with_unit) :-
  parse_ingredient("200 g fluffy bits, thinly sliced", ingredient(200, g, "fluffy bits")).

test(with_unit_attached) :-
  parse_ingredient("200g fluffy bits, thinly sliced", ingredient(200, g, "fluffy bits")).

test(without_unit) :-
  parse_ingredient("200 fluffy bits, thinly sliced", ingredient(200, "fluffy bits")).

test(without_quantity) :-
  parse_ingredient("fluffy bits, thinly sliced", ingredient("fluffy bits")).

test(with_dot_quantity) :-
  parse_ingredient("2.5 fluffy bits", ingredient(2.5, "fluffy bits")).

test(with_comma_quantity) :-
  parse_ingredient("2,5 fluffy bits", ingredient(2.5, "fluffy bits")).

test(with_range_quantity) :-
  parse_ingredient("2-3 fluffy bits", ingredient(2.5, "fluffy bits")).

test(with_fractional_quantity) :-
  parse_ingredient("1/4 fluffy bits", ingredient(0.25, "fluffy bits")).

test(with_utf8_fractions) :-
  parse_ingredient("½ fluffy bits", ingredient(0.5, "fluffy bits")).

test(with_diminuitive_unit) :-
  parse_ingredient("1 kopje fluffy bits", ingredient(1, kop, "fluffy bits")).

test(with_weirdly_cased_unit) :-
  parse_ingredient("1 LiTER fluffy bits", ingredient(1, l, "fluffy bits")).

:- end_tests(parse_ingredient).

parse_ingredient(String, I) :-
  (
    split_string(String, " ", "", [QuantityWord, UnitString | IngredientWords]),
    atom_string(UnitWord, UnitString),
    quantity_string(Quantity, QuantityWord),
    unit(Unit, UnitWord)
  ) ->
  (
    atomics_to_string(IngredientWords, ' ', DirtyIngredient),
    up_to_comma(DirtyIngredient, Ingredient),
    I = ingredient(Quantity, Unit, Ingredient)
  );
  (
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    quantity_with_unit(QuantityWord, Quantity, Unit)
  ) ->
  (
    atomics_to_string(IngredientWords, ' ', DirtyIngredient),
    up_to_comma(DirtyIngredient, Ingredient),
    I = ingredient(Quantity, Unit, Ingredient)
  );
  (
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    quantity_string(Quantity, QuantityWord)
  ) ->
  (
    atomics_to_string(IngredientWords, ' ', DirtyIngredient),
    up_to_comma(DirtyIngredient, Ingredient),
    I = ingredient(Quantity, Ingredient)
  );
  (
    up_to_comma(String, Ingredient),
    I = ingredient(Ingredient)
  ).

quantity_string(0.5, "½").
quantity_string(0.25, "¼").
quantity_string(0.5, Halve) :-
  string_lower(Halve, "halve").
quantity_string(Quantity, String) :-
  number_string(Quantity, String) ->
  true;
  (
    re_replace(",", ".", String, DottedString),
    number_string(Quantity, DottedString)
  ) ->
  true;
  (
    split_string(String, "/", "", [NumeratorString, DenominatorString]),
    number_string(Numerator, NumeratorString),
    number_string(Denominator, DenominatorString),
    Quantity is Numerator / Denominator
  ) ->
  true;
  (
    split_string(String, "-", "", [LowerString, UpperString]),
    number_string(Lower, LowerString),
    number_string(Upper, UpperString),
    Quantity is (Lower + Upper) / 2
  ).

% Provide the part of the sentence up to the first comma.
up_to_comma(Sentence, UpToComma) :-
  split_string(Sentence, ",", "", [UpToComma|_]) ->
    true;
    Sentence = UpToComma.

quantity_with_unit(String, Quantity, Unit) :-
  string_concat(QuantityString, UnitString, String),
  atom_string(UnitWord, UnitString),
  unit(Unit, UnitWord),
  number_string(Quantity, QuantityString).

unit(Unit, UnitString) :-
  string_lower(UnitString, UnitStringLower),
  unit_helper(Unit, UnitStringLower).

unit_helper(Unit, Unit) :- base_unit(Unit).
unit_helper(Unit, UnitWord) :-
  base_unit(Unit),
  string_concat(Unit, _, UnitWord).
unit_helper(kg, kilogram).
unit_helper(g, gram).
unit_helper(l, liter).
unit_helper(tl, theelepel).
unit_helper(el, eetlepel).

% SI units
base_unit(g).
base_unit(ml).
base_unit(cl).
base_unit(dl).
base_unit(l).
base_unit(kg).

% Dutch 'units'
base_unit(tl).
base_unit(el).
base_unit(kop).
base_unit(teen).
base_unit(tak).
base_unit(bos).
base_unit(mespunt).
base_unit(blik).
