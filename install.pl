:- set_prolog_flag(verbose, silent).
:- initialization(main).

main :-
  make_directory_path('packs/'),
  pack_install('markdown', [package_directory('packs/') , interactive(false)]),
  halt.
main :-
  halt(1).
