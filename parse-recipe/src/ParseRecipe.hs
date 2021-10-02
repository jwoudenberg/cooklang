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
  let emptyRecipe = Recipe (Compose (pure Nothing)) (Compose (pure Nothing)) (Compose (pure Nothing)) (Compose (pure Nothing))
  let recipePandoc = foldr parseBlock emptyRecipe blocks
  recipeMaybe <- runIOorExplode $ extract getCompose recipePandoc
  recipe <-
    case extract (fmap Identity) recipeMaybe of
      Nothing -> fail "Recipe was missing section"
      Just recipe' -> pure recipe'
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

parseBlock :: Block -> Recipe (Compose PandocIO Maybe) -> Recipe (Compose PandocIO Maybe)
parseBlock block recipe =
  case block of
    Header 1 _ inlines ->
      recipe {name = Compose (fmap Just (toText [Plain inlines]))}
    Para _ ->
      recipe {portions = Compose (pure (Just 1))}
    BulletList items ->
      recipe {ingredients = Compose (fmap Just (traverse toText items))}
    OrderedList _ items ->
      recipe {instructions = Compose (fmap Just (traverse toText items))}
    _ -> recipe

toText :: [Block] -> PandocIO Text
toText blocks = writePlain def (Pandoc mempty blocks)
