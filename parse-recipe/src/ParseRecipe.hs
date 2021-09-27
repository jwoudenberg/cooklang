{-# LANGUAGE RankNTypes #-}

module ParseRecipe where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import qualified Data.Text.IO
import Text.Pandoc

main :: IO ()
main = do
  markdownText <- Data.Text.IO.readFile testFile
  (Pandoc _ blocks) <- runIOorExplode $ readMarkdown def markdownText
  let emptyRecipe = Recipe (Compose Nothing) (Compose Nothing) (Compose Nothing) (Compose Nothing)
  let recipeMaybe = foldr parseBlock emptyRecipe blocks
  recipePandoc <-
    case extract getCompose recipeMaybe of
      Nothing -> fail "Recipe was missing section"
      Just recipe' -> pure recipe'
  recipe <- extract (fmap Identity . runIOorExplode) recipePandoc
  print (name recipe :: Identity Text)

testFile :: FilePath
testFile = "../../hiskevriendelijk/docs/hoofdgerechten/fesenjoon.md"

data Recipe m = Recipe
  { name :: m Text,
    portions :: m Int,
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

parseBlock :: Block -> Recipe (Compose Maybe PandocIO) -> Recipe (Compose Maybe PandocIO)
parseBlock block recipe =
  case block of
    Header 1 _ inlines ->
      recipe {name = Compose (Just (toText [Plain inlines]))}
    Para _ ->
      recipe {portions = Compose (Just (pure 1))}
    BulletList items ->
      recipe {ingredients = Compose (Just (traverse toText items))}
    OrderedList _ items ->
      recipe {instructions = Compose (Just (traverse toText items))}
    _ -> recipe

toText :: [Block] -> PandocIO Text
toText blocks = writePlain def (Pandoc mempty blocks)
