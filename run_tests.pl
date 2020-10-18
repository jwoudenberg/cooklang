#!/usr/bin/env swipl

:- initialization(main).
:- use_module(groceries).

main :-
  run_tests(),
  halt.
main :-
  halt(1).
