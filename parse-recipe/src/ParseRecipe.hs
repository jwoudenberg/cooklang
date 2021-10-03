{-# LANGUAGE RankNTypes #-}

module ParseRecipe where

import qualified Data.Attoparsec.Text as P
import Data.Foldable (for_, traverse_)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.List (intersperse)
import Data.Text (Text, strip, unpack)
import qualified Data.Text.IO
import qualified System.Environment
import qualified System.Exit
import System.IO (Handle, hPutStr, stderr, stdout)
import Text.Pandoc

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--help"] -> showHelp
    [path] -> run path
    _ -> do
      showHelp
      System.Exit.exitFailure

showHelp :: IO ()
showHelp = do
  Data.Text.IO.hPutStrLn stderr "parse-recipe RECIPE"
  Data.Text.IO.hPutStrLn stderr "Parses a markdown file containing a recipe and prints a datalog representation of its ingredients to stdout."

run :: FilePath -> IO ()
run recipeFile = do
  markdownText <- Data.Text.IO.readFile recipeFile
  (Pandoc _ blocks) <- runIOorExplode $ readMarkdown def markdownText
  let emptyRecipe =
        Recipe
          { title = Compose (pure (Left "Did not find recipe title.")),
            portions = Compose (pure (Right Nothing)),
            ingredients = (Compose (pure (Right []))),
            instructions = Compose (pure (Right []))
          }
  let recipePandoc = foldr parseBlock emptyRecipe blocks
  recipeMaybe <- runIOorExplode $ extract getCompose recipePandoc
  recipe <-
    case extract (fmap Identity) recipeMaybe of
      Left err -> System.Exit.die (unpack err)
      Right recipe' -> pure recipe'
  printDatalogProgram stdout (recipeFacts recipe)

data Recipe m = Recipe
  { title :: m Text,
    portions :: m (Maybe Double),
    ingredients :: m [Ingredient],
    instructions :: m [Text]
  }

data Ingredient = Ingredient
  { name :: Text,
    quantity :: Maybe Quantity
  }
  deriving (Show)

data Quantity = Quantity
  { amount :: Double,
    unit :: Maybe Unit
  }
  deriving (Show)

data Unit
  = Milli SiUnit
  | Centi SiUnit
  | Deci SiUnit
  | Kilo SiUnit
  | SiUnit SiUnit
  | Eetlepel
  | Theelepel
  | Snuf
  deriving (Show)

data SiUnit
  = Gram
  | Liter
  deriving (Show)

extract :: Applicative g => (forall a. f a -> g (h a)) -> Recipe f -> g (Recipe h)
extract f recipe =
  Recipe
    <$> f (title recipe)
    <*> f (portions recipe)
    <*> f (ingredients recipe)
    <*> f (instructions recipe)

parseBlock :: Block -> Recipe (Compose PandocIO (Either Text)) -> Recipe (Compose PandocIO (Either Text))
parseBlock block recipe =
  case block of
    Header 1 _ inlines ->
      recipe {title = Compose (fmap Right (toText [Plain inlines]))}
    Para inlines ->
      recipe
        { portions = Compose $ do
            paragraphText <- toText [Plain inlines]
            pure . Right $ P.maybeResult (P.parse portionsParser paragraphText)
        }
    BulletList items ->
      recipe
        { ingredients =
            Compose $ fmap Right $ traverse (fmap parseIngredient . toText) items
        }
    OrderedList _ items ->
      recipe {instructions = Compose (fmap Right (traverse toText items))}
    _ -> recipe

toText :: [Block] -> PandocIO Text
toText blocks = fmap strip $ writePlain def (Pandoc mempty blocks)

portionsParser :: P.Parser Double
portionsParser =
  P.asciiCI "Voor "
    *> fmap realToFrac P.scientific
    <* P.choice [P.asciiCI " personen", P.asciiCI " persoon"]

parseIngredient :: Text -> Ingredient
parseIngredient ingredientText =
  case P.parse (quantityParser <* P.space) ingredientText of
    P.Fail _ _ _ -> Ingredient {name = ingredientText, quantity = Nothing}
    P.Partial _ -> Ingredient {name = ingredientText, quantity = Nothing}
    P.Done name' quantity' -> Ingredient {name = name', quantity = Just quantity'}

quantityParser :: P.Parser Quantity
quantityParser =
  P.choice
    [ Quantity <$> amountParser <* P.skipSpace <*> (fmap Just unitParser), -- 200 g bonen
      Quantity <$> pure 1 <* P.skipSpace <*> (fmap Just unitParser), -- snufje zout
      Quantity <$> amountParser <*> pure Nothing -- 2 paprikas
    ]

amountParser :: P.Parser Double
amountParser =
  P.choice
    [ fractionParser,
      fmap realToFrac P.scientific
    ]

fractionParser :: P.Parser Double
fractionParser = do
  enumerator <- P.scientific
  P.skipSpace
  _ <- P.string "/"
  P.skipSpace
  denumerator <- P.scientific
  pure (realToFrac enumerator / realToFrac denumerator)

unitParser :: P.Parser Unit
unitParser =
  P.choice
    [ SiUnit <$> siUnitParser,
      prefixedSiUnit Milli "milli",
      prefixedSiUnit Milli "m",
      prefixedSiUnit Centi "centi",
      prefixedSiUnit Centi "c",
      prefixedSiUnit Deci "deci",
      prefixedSiUnit Deci "d",
      prefixedSiUnit Kilo "kilo",
      prefixedSiUnit Kilo "k",
      optionallyPlural $ colloquial Eetlepel "eetlepel",
      colloquial Eetlepel "el",
      optionallyPlural $ colloquial Theelepel "theelepel",
      colloquial Theelepel "tl",
      optionallyDiminuitive $ colloquial Snuf "snuf"
    ]

prefixedSiUnit :: (SiUnit -> Unit) -> Text -> P.Parser Unit
prefixedSiUnit ctor prefix =
  ctor <$> (P.asciiCI prefix *> siUnitParser)

colloquial :: Unit -> Text -> P.Parser Unit
colloquial unit base =
  pure unit <* P.asciiCI base

optionallyPlural :: P.Parser a -> P.Parser a
optionallyPlural parser =
  P.choice
    [ parser <* P.asciiCI "s",
      parser
    ]

optionallyDiminuitive :: P.Parser a -> P.Parser a
optionallyDiminuitive parser =
  P.choice
    [ parser <* P.asciiCI "jes",
      parser <* P.asciiCI "je",
      parser <* P.asciiCI "tjes",
      parser <* P.asciiCI "tje",
      parser
    ]

siUnitParser :: P.Parser SiUnit
siUnitParser =
  P.choice
    [ P.asciiCI "gram" *> pure Gram,
      P.asciiCI "g" *> pure Gram,
      P.asciiCI "liter" *> pure Liter,
      P.asciiCI "l" *> pure Liter
    ]

data DatalogFact = DatalogFact
  { relation :: Text,
    constants :: [DatalogConstant]
  }

data DatalogConstant
  = String Text
  | Number Double

recipeFacts :: Recipe Identity -> [DatalogFact]
recipeFacts recipe =
  let recipeName = runIdentity (title recipe)
      ingredientFacts = fmap (ingredientFact recipeName) (runIdentity (ingredients recipe))
   in case runIdentity (portions recipe) of
        Nothing -> ingredientFacts
        Just portions' ->
          DatalogFact
            "portions"
            [ String recipeName,
              Number portions'
            ] :
          ingredientFacts

ingredientFact :: Text -> Ingredient -> DatalogFact
ingredientFact recipe ingredient =
  case ingredient of
    Ingredient {name, quantity = Nothing} ->
      DatalogFact "ingredient" [String recipe, String name]
    Ingredient {name, quantity = Just Quantity {amount, unit = Nothing}} ->
      DatalogFact "ingredient" [String recipe, Number amount, String name]
    Ingredient {name, quantity = Just Quantity {amount, unit = Just unit'}} ->
      DatalogFact "ingredient" [String recipe, Number amount, String (unitToText unit'), String name]

unitToText :: Unit -> Text
unitToText unit' =
  case unit' of
    Milli siUnit -> "m" <> siUnitToText siUnit
    Centi siUnit -> "c" <> siUnitToText siUnit
    Deci siUnit -> "d" <> siUnitToText siUnit
    Kilo siUnit -> "k" <> siUnitToText siUnit
    SiUnit siUnit -> siUnitToText siUnit
    Eetlepel -> "el"
    Theelepel -> "tl"
    Snuf -> "snuf"

siUnitToText :: SiUnit -> Text
siUnitToText siUnit =
  case siUnit of
    Gram -> "g"
    Liter -> "l"

printDatalogProgram :: Handle -> [DatalogFact] -> IO ()
printDatalogProgram handle facts =
  let p = Data.Text.IO.hPutStr handle
      printFact fact =
        case fact of
          Number n -> hPutStr handle (show n)
          String string -> p "\"" *> p string *> p "\""
   in for_ facts $ \fact -> do
        p (relation fact)
        p "("
        traverse_ id (intersperse (p ", ") (printFact <$> constants fact))
        p ").\n"
