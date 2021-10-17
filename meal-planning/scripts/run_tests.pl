#!/usr/bin/env swipl

:- initialization(main).
:- use_module(example/recipes).
:- use_module(lib/groceries).

main :-
  run_tests(),
  halt.
main :-
  halt(1).
