{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ShoppingTool where

import          Data.Aeson 
import          Data.ByteString                (readFile, ByteString)
import          Data.ByteString.Lazy           (toStrict)
import          Data.Maybe                     (fromMaybe)
import          Data.Text                      (Text, pack)
import          GHC.Generics                   (Generic)
import          Network.HTTP.Req
import          Data.Text.Encoding             ( decodeUtf8 )


storeNames = ["Hofer", "Tus", "Spar", "Mercator"]

data ShoppingItem = ShoppingItem { name           :: Text
                 , hoferPrice     :: Double
                 , tusPrice       :: Double
                 , sparPrice      :: Double
                 , mercatorPrice  :: Double
                 } deriving (Generic, FromJSON, ToJSON, Show)

getJsonFile :: IO ByteString
getJsonFile = runReq defaultHttpConfig $ do
  response <- req
    GET -- method
    (  https "rekkuso.github.io"
    /: "rekkuso-bot"
    /: "pricedata.json"
    ) -- safe by construction URL
    (NoReqBody)
    bsResponse $ -- specify how to interpret response
    header "User-Agent" "AdsBot-Google (+http://www.google.com/adsbot.html)"
  pure $ responseBody response

getShoppingItemList :: IO [ShoppingItem]
getShoppingItemList = do
  itemFile <- getJsonFile
  let maybeShoppingItemList = decodeStrict itemFile
  let itemList = fromMaybe [] maybeShoppingItemList
  pure itemList

getShoppingItem :: Text -> [ShoppingItem] -> Maybe ShoppingItem
getShoppingItem search list = do
  let match = filter (\item -> name item == search) list
  if null match then Nothing else Just $ head match

getShoppingItemPrices :: ShoppingItem -> [(Text, Double)]
getShoppingItemPrices ShoppingItem {..} = zip storeNames [hoferPrice, tusPrice, sparPrice, mercatorPrice]
