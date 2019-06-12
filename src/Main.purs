module Main where

import Data.Array
import Data.Tuple
import Partial.Unsafe
import Prelude
import Types

import Control.Monad.Except (runExcept)
import Control.Parallel (parTraverse, parTraverse_)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (Error)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Generic.Class (decode, encode)

foreign import callApi :: Fn3 Foreign (Error → Effect Unit) (Foreign → Effect Unit) (Effect Unit)

main :: Effect Unit
main = do
  launchAff_ $ do
     val ← take 10 <$> getTopStories
     parTraverse_ printAllDetails val
  log "Hello sailor!"

printAllDetails :: Int → Aff Unit
printAllDetails id = do
  item ← getItem id
  case item of
       Just (ItemResponse i) → do
          case i.kids of
               Just k → do
                  allUsers     <- parTraverse (handleUser i) k
                  allComments  <- pure $ foldl (\acc (Tuple a b) → acc <> b) [] allUsers
                  liftEffect <<<  log $ "\nAll Comments for: " <> (unsafePartial $ fromJust i.title)
                  liftEffect $ logShow allComments
                  traverse_ (\(Tuple a _) → liftEffect $ do
                                log $ "Top 10 comments by: " <> fst a
                                logShow $ snd a) allUsers
               Nothing → liftEffect <<< log $ "No Comments available for :" <> (unsafePartial $ fromJust i.title)
       Nothing → liftEffect <<< log $ "No Item Available with id: " <> (show id)

handleUser :: _ → Int → Aff (Tuple (Tuple String (Array Int)) (Array Int))
handleUser parentItem id = do
  item ← getItem id
  case item of
       Nothing → mempty
       Just (ItemResponse i) → do
          user ← getUser i.by
          pure $ case user of
                    Just (UserResponse ur) → Tuple (Tuple i.by (take 10 ur.submitted)) (intersect ur.submitted (unsafePartial $ fromJust parentItem.kids))
                    Nothing → Tuple (Tuple i.by mempty) (unsafePartial $ fromJust parentItem.kids)

getUser :: String → Aff (Maybe UserResponse)
getUser userName = do
  let req = defaultMakeRequest_ (url userS <> userName <>".json") (method userS)
  resp ← makeAff (\cb → runFn3 callApi (encode req) (Left >>> cb) (Right >>> cb) *> pure nonCanceler)
  case runExcept <<< decode $ resp of
       Right v → pure (Just v)
       Left err → pure Nothing

getItem :: Int → Aff (Maybe ItemResponse)
getItem id = do
  let req = defaultMakeRequest_ (url itemS <> show id <> ".json") (method itemS)
  resp ← makeAff (\cb → runFn3 callApi (encode req) (Left >>> cb) (Right >>> cb) *> pure nonCanceler)
  case runExcept <<< decode $ resp of
       Right v → pure (Just v)
       Left err → pure Nothing

getTopStories :: Aff (Array Int)
getTopStories = do
  let req  = defaultMakeRequest_ (url topStories) (method topStories)
  resp ← makeAff (\cb → runFn3 callApi (encode req) (Left >>> cb) (Right >>> cb) *> pure nonCanceler)
  case runExcept <<< decode $ resp of
       Right v → pure v
       Left err → pure []
 where
       emptyForeign = unsafeToForeign Nothing

defaultMakeRequest_ urlV met =
      Request { url: urlV
              , method: met
              , headers: Headers []
              , payload: ""
              }


userS = SProxy :: SProxy "user"
itemS = SProxy :: SProxy "item"
topStories = SProxy :: SProxy "topstories"
