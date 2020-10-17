:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- use_module(recipes).

main :-
  current_prolog_flag(argv, Argv),
  open('recipes-from-markdown.pl', write, Stream),
  maplist(parse_recipe(Stream), Argv),
  close(Stream),
  halt.
main :-
  halt(1).
