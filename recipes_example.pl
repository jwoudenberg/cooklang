:- module(recipes_example, [portions/2, contains/2]).
:- style_check(-discontiguous).

portions("Spagetti Bolognese",4).
contains("Spagetti Bolognese",ingredient(500,g,"minced beef")).
contains("Spagetti Bolognese",ingredient(100,g,"ham")).
contains("Spagetti Bolognese",ingredient(1,"onion")).
contains("Spagetti Bolognese",ingredient(1,toe,"garlic")).
contains("Spagetti Bolognese",ingredient(2,cans,"peeled tomatoes")).
contains("Spagetti Bolognese",ingredient("small can of taomatoo puree")).
contains("Spagetti Bolognese",ingredient(1,dl,"red whine")).
contains("Spagetti Bolognese",ingredient(1,"bay leaf")).
contains("Spagetti Bolognese",ingredient("oregano")).
contains("Spagetti Bolognese",ingredient("salt, peper, paprika")).
contains("Spagetti Bolognese",ingredient("grated cheese")).
contains("Spagetti Bolognese",ingredient(500,g,"spaghetti")).

portions("Egg curry with broccoli",2).
contains("Egg curry with broccoli",ingredient(4,"eggs")).
contains("Egg curry with broccoli",ingredient(600,g,"broccoli")).
contains("Egg curry with broccoli",ingredient(3,tbsp,"olive oil")).
contains("Egg curry with broccoli",ingredient(1,"onion")).
contains("Egg curry with broccoli",ingredient(2,tbsp,"curry powder")).
contains("Egg curry with broccoli",ingredient(400,ml,"coconut milk")).
contains("Egg curry with broccoli",ingredient(100,g,"cashews")).
