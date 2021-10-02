{-# LANGUAGE RankNTypes #-}

module ParseRecipe where

import qualified Data.Attoparsec.Text as P
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Text (Text, strip)
import qualified Data.Text.IO
import Text.Pandoc

main :: IO ()
main = do
  markdownText <- Data.Text.IO.readFile testFile
  (Pandoc _ blocks) <- runIOorExplode $ readMarkdown def markdownText
  let emptyRecipe = Recipe (Compose (pure Nothing)) (Compose (pure Nothing)) (Compose (pure Nothing)) (Compose (pure Nothing))
  let recipePandoc = foldr parseBlock emptyRecipe blocks
  recipeMaybe <- runIOorExplode $ extract getCompose recipePandoc
  recipe <-
    case extract (fmap Identity) recipeMaybe of
      Nothing -> fail "Recipe was missing section"
      Just recipe' -> pure recipe'
  print (ingredients recipe)

testFile :: FilePath
testFile = "../../hiskevriendelijk/docs/hoofdgerechten/fesenjoon.md"

data Recipe m = Recipe
  { title :: m Text,
    portions :: m Double,
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

parseBlock :: Block -> Recipe (Compose PandocIO Maybe) -> Recipe (Compose PandocIO Maybe)
parseBlock block recipe =
  case block of
    Header 1 _ inlines ->
      recipe {title = Compose (fmap Just (toText [Plain inlines]))}
    Para inlines ->
      recipe
        { portions = Compose $ do
            paragraphText <- toText [Plain inlines]
            pure $ P.maybeResult (P.parse portionsParser paragraphText)
        }
    BulletList items ->
      recipe
        { ingredients =
            Compose $ fmap Just $ traverse (fmap parseIngredient . toText) items
        }
    OrderedList _ items ->
      recipe {instructions = Compose (fmap Just (traverse toText items))}
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
