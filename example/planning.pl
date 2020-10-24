:- use_module(example/recipes).
:- use_module(lib/groceries).

favorite("bolognese").
favorite("curry").

vegi(R) :- without(R, meat).

meat("chicken").
meat("beef").
meat("pork").
meat("lamb").
