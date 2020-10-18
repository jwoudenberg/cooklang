#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- set_test_options([load(never)]).
:- initialization(main).
:- use_module(groceries).

main :-
  current_prolog_flag(argv, Argv),
  open('recipes.pl', write, Stream),
  format(Stream, ":- module(recipes, [portions/2, contains/2]).~n~n", []),
  format(Stream, ":- style_check(-discontiguous).~n", []),
  maplist(parse_recipe(Stream), Argv),
  close(Stream),
  halt.
main :-
  halt(1).
