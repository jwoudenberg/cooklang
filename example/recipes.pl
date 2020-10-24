:- module(recipes, [portions/2, contains/2]).
:- style_check(-discontiguous).

portions("Spaghetti Bolognese",4).
contains("Spaghetti Bolognese",ingredient(500,g,"minced beef")).
contains("Spaghetti Bolognese",ingredient(100,g,"ham")).
contains("Spaghetti Bolognese",ingredient(1,"onion")).
contains("Spaghetti Bolognese",ingredient(1,toe,"garlic")).
contains("Spaghetti Bolognese",ingredient(2,cans,"peeled tomatoes")).
contains("Spaghetti Bolognese",ingredient("small can of tomato puree")).
contains("Spaghetti Bolognese",ingredient(1,dl,"red whine")).
contains("Spaghetti Bolognese",ingredient(1,"bay leaf")).
contains("Spaghetti Bolognese",ingredient("oregano")).
contains("Spaghetti Bolognese",ingredient("salt, peper, paprika")).
contains("Spaghetti Bolognese",ingredient("grated cheese")).
contains("Spaghetti Bolognese",ingredient(500,g,"spaghetti")).

portions("Egg curry with broccoli",2).
contains("Egg curry with broccoli",ingredient(4,"eggs")).
contains("Egg curry with broccoli",ingredient(600,g,"broccoli")).
contains("Egg curry with broccoli",ingredient(3,tbsp,"olive oil")).
contains("Egg curry with broccoli",ingredient(1,"onion")).
contains("Egg curry with broccoli",ingredient(2,tbsp,"curry powder")).
contains("Egg curry with broccoli",ingredient(400,ml,"coconut milk")).
contains("Egg curry with broccoli",ingredient(100,g,"cashews")).
