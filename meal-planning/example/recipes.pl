:- module(recipes, [portions/2, ingredient/2, ingredient/3, ingredient/4]).
:- style_check(-discontiguous).

portions("Spaghetti Bolognese",4).
ingredient("Spaghetti Bolognese",500,g,"minced beef").
ingredient("Spaghetti Bolognese",100,g,"ham").
ingredient("Spaghetti Bolognese",1,"onion").
ingredient("Spaghetti Bolognese",1,toe,"garlic").
ingredient("Spaghetti Bolognese",2,cans,"peeled tomatoes").
ingredient("Spaghetti Bolognese","small can of tomato puree").
ingredient("Spaghetti Bolognese",1,dl,"red whine").
ingredient("Spaghetti Bolognese",1,"bay leaf").
ingredient("Spaghetti Bolognese","oregano").
ingredient("Spaghetti Bolognese","salt").
ingredient("Spaghetti Bolognese","grated cheese").
ingredient("Spaghetti Bolognese",500,g,"spaghetti").

portions("Egg curry with broccoli",2).
ingredient("Egg curry with broccoli",4,"eggs").
ingredient("Egg curry with broccoli",600,g,"broccoli").
ingredient("Egg curry with broccoli",3,tbsp,"olive oil").
ingredient("Egg curry with broccoli",1,"onion").
ingredient("Egg curry with broccoli",2,tbsp,"curry powder").
ingredient("Egg curry with broccoli",400,ml,"coconut milk").
ingredient("Egg curry with broccoli",100,g,"cashews").
