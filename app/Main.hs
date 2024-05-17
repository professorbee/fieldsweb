{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy as Lazy
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Heterocephalus
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type ItemApi =
    Header "Host" Text :> Get '[HTML] RawHtml
    :<|> "item" :> Get '[JSON] [Item]
    :<|> "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

homePage :: Item -> Text -> ByteString
homePage item host = renderMarkup $(compileTextFile "templates/hello.html")

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
          setBeforeMainLoop
            (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  rootItem
    :<|> getItems
    :<|> getItemById

rootItem :: Maybe Text -> Handler RawHtml
rootItem host = return $ RawHtml $ homePage exampleItem $ fromMaybe "" host

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \case
  0 -> return exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

data Item
  = Item
  { itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item

instance FromJSON Item
