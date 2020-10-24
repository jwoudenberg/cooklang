:- use_module(recipes).
:- use_module(lib/groceries).

favorite("vega bolognese").
favorite("rajma").
favorite("fesenjoon").
favorite("dahl").
favorite("hutspot").

vega(R) :- without(R, vlees).

vlees("kip").
vlees("rund").
vlees("varken").
vlees("lam").
