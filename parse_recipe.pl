:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- use_module(recipes).

main :-
  current_prolog_flag(argv, [Argv]),
  parse_recipe(Argv),
  halt.
main :-
  halt(1).
