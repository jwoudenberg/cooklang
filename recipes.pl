:-style_check(-discontiguous).

% Bolognese

recipe("Spaghetti bolognese a la mama", bolognese).
portions(bolognese, 4).
contains(bolognese, "1 blikje tomatenblokjes").
contains(bolognese, "100 g spaghetti").
contains(bolognese, "400g gehakt").

% Aubergineschotel

recipe("Aubergineschotel met kaas", aubergineschotel).
portions(aubergineschotel, 2).
contains(aubergineschotel, "1.5 aubergines").
contains(aubergineschotel, "200g kaas").
contains(aubergineschotel, "1 blikje tomatenblokjes").

% Helpers

full_name(Part, Full) :-
  recipe(Full, _),
  string_lower(Full, FullLower),
  first_sub_string(FullLower, Part).

first_sub_string(Full, Part) :-
  sub_string(Full, _, _, _, Part),
  !.

ingredients(Recipe, Ingredients) :-
  full_name(Recipe, FullName),
  recipe(FullName, ShortHand),
  findall(Ingredient, contains(ShortHand, Ingredient), Ingredients).

ingredient(String, Quantity, Unit, Ingredient) :-
  (
    split_string(String, " ", "", [QuantityWord | IngredientWords]),
    quantity_with_unit(QuantityWord, Quantity, Unit),
    atomics_to_string(IngredientWords, ' ', Ingredient)
  );
  (
    split_string(String, " ", "", [QuantityWord, UnitWord | IngredientWords]),
    atom_string(Unit, UnitWord),
    unit(Unit),
    number_string(Quantity, QuantityWord),
    atomics_to_string(IngredientWords, ' ', Ingredient)
  ).

quantity_with_unit(String, Quantity, Unit) :-
  string_concat(QuantityString, UnitString, String),
  atom_string(Unit, UnitString),
  unit(Unit),
  number_string(Quantity, QuantityString).

unit(g).
