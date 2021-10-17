:- use_module(example/recipes).
:- use_module(lib/groceries).

favorite("bolognese").
favorite("curry").

vegi(R) :- without(R, meat).

meat("chicken").
meat("beef").
meat("pork").
meat("lamb").

grocery_list_from(date(2020,07,12)).

planned(date(2020,11,14), "bolognese", 2).
planned(date(2020,11,11), "curry", 2).
planned(date(2020,08,12), "curry", 2).
