module Types where

import Data.Symbol

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Generic (class Encode, class Decode, genericEncode, genericDecode, defaultOptions)
import Prelude (class Eq, class Show, eq, (<>))

baseUrl :: String
baseUrl = "https://hacker-news.firebaseio.com/v0/"

class (IsSymbol sym) <= RestEndpoint sym where
  url :: SProxy sym → String
  method :: SProxy sym → String

instance topStories :: RestEndpoint "topstories" where
  url _ = baseUrl <> "topstories.json"
  method _ = "get"

instance item :: RestEndpoint "item" where
  url _ = baseUrl <> "item/"
  method _ = "get"

instance user :: RestEndpoint "user" where
  url _ =  baseUrl <> "user/"
  method _ = "get"

type HeaderField = String
type HeaderValue = String
data Header = Header HeaderField HeaderValue
newtype Headers = Headers (Array Header)

derive instance genericHeader :: Generic Header _
instance encodeHeaderG :: Encode Header where
  encode = genericEncode (defaultOptions {unwrapSingleConstructors = true})

derive instance genericHeaders :: Generic Headers _
instance encodeHeadersG :: Encode Headers where
  encode = genericEncode (defaultOptions {unwrapSingleConstructors = true})

newtype Request = Request
  { url :: String
  , method :: String
  , headers :: Headers
  , payload :: String
  }

derive instance genericRequest :: Generic Request _
instance encodeRequestG :: Encode Request where
  encode = genericEncode (defaultOptions {unwrapSingleConstructors = true})

newtype ItemResponse = ItemResponse
  { kids :: Maybe (Array Int)
  , title :: Maybe String
  , by :: String
  }

derive instance genItemResponse :: Generic ItemResponse _
instance showItemResponse :: Show ItemResponse where show = genericShow

instance decodeItemResponse :: Decode ItemResponse where
  decode = genericDecode (defaultOptions {unwrapSingleConstructors = true})

instance eqItemResponse :: Eq ItemResponse where
  eq (ItemResponse a) (ItemResponse b) = eq a.by b.by


newtype UserResponse = UserResponse
  { karma :: Int
  , id :: String
  , submitted :: Array Int
  }

derive instance genUserResponse :: Generic UserResponse _
instance showUserResponse :: Show UserResponse where show = genericShow

instance decodeUserResponse :: Decode UserResponse where
  decode = genericDecode (defaultOptions {unwrapSingleConstructors = true})

instance eqUserResponse :: Eq UserResponse where
  eq (UserResponse a) (UserResponse b) = eq a.id b.id
