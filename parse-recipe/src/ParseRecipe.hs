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
  { name :: m Text,
    portions :: m Float,
    ingredients :: m [Text],
    instructions :: m [Text]
  }

extract :: Applicative g => (forall a. f a -> g (h a)) -> Recipe f -> g (Recipe h)
extract f recipe =
  Recipe
    <$> f (name recipe)
    <*> f (portions recipe)
    <*> f (ingredients recipe)
    <*> f (instructions recipe)

parseBlock :: Block -> Recipe (Compose PandocIO Maybe) -> Recipe (Compose PandocIO Maybe)
parseBlock block recipe =
  case block of
    Header 1 _ inlines ->
      recipe {name = Compose (fmap Just (toText [Plain inlines]))}
    Para inlines ->
      recipe
        { portions = Compose $ do
            paragraphText <- toText [Plain inlines]
            case P.parse parsePortions paragraphText of
              P.Fail _ _ _ -> pure Nothing
              P.Partial _ -> pure Nothing
              P.Done _ portions' -> pure (Just portions')
        }
    BulletList items ->
      recipe {ingredients = Compose (fmap Just (traverse toText items))}
    OrderedList _ items ->
      recipe {instructions = Compose (fmap Just (traverse toText items))}
    _ -> recipe

toText :: [Block] -> PandocIO Text
toText blocks = fmap strip $ writePlain def (Pandoc mempty blocks)

parsePortions :: P.Parser Float
parsePortions =
  P.asciiCI "Voor "
    *> fmap realToFrac P.scientific
    <* P.choice [P.asciiCI " personen", P.asciiCI " persoon"]
