#!/usr/bin/env swipl

:- initialization(main).
:- use_module(lib/recipe_parsing).

main :-
  run_tests(),
  halt.
main :-
  halt(1).
