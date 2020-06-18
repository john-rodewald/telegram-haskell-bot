{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module EbayKScraper where

import           Data.Aeson
import           Data.ByteString               as BS
                                                ( ByteString, writeFile )
import           Control.Monad.IO.Class         ( liftIO )

import           Data.ByteString.Lazy          as BSL
                                                ( toStrict )
import           Data.List (intersperse)
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as Text
                                                ( Text, pack, concat )
import           Data.Text.Encoding             ( decodeUtf8 )
import           GHC.Generics
import           Text.HTML.Scalpel
import           Network.HTTP.Req


type Card = Text

data EbayKAd = EbayKAd { title    :: Text
                       , desc     :: Text
                       , price    :: Text
                       , date     :: Text
                       , location :: [Text]
                       , url      :: Text } deriving (Generic, ToJSON, FromJSON, Show)

extractFrom = scrapeStringLike

getEbayKAds :: [Text] -> IO (BS.ByteString)
getEbayKAds query = do
  maybeCards <- scrapeCards query
  let cards = fromMaybe [] maybeCards

  let ads = map
        (\card -> EbayKAd { title     = (cardTitle card)
                          , desc      = (cardDesc card)
                          , price     = (cardPrice card)
                          , date      = (cardDate card)
                          , location  = (cardLocation card)
                          , url       = (cardUrl card)
                          }
        )
        (take 5 cards)
  let adsStr = encode ads
  pure $ toStrict adsStr

getPage :: [Text] -> IO Text
getPage query = runReq defaultHttpConfig $ do
  response <- req
    GET
    (  https "ebay-kleinanzeigen.de"
    /: ("s-" <> (Text.concat $ intersperse "-" query))
    /: "k0"
    )
    (NoReqBody)
    bsResponse $
    header "User-Agent" "AdsBot-Google (+http://www.google.com/adsbot.html)"
  pure $ decodeUtf8 $ responseBody response

scrapeCards :: [Text] -> IO (Maybe [Card])
scrapeCards query = do
  let scraper = htmls ("article" @: [hasClass "aditem"])
  page <- getPage query
  let adCards' = extractFrom page scraper
  pure adCards' 

cardTitle :: Card -> Text
cardTitle card = title'
 where
  maybeTitle = innerHTML ("a" @: [hasClass "ellipsis"])
  title'     = fromMaybe "" (extractFrom card maybeTitle)

cardDesc :: Card -> Text
cardDesc card = desc'
 where
  maybeDesc = innerHTML ("article" // "p")
  desc'     = fromMaybe "" (extractFrom card maybeDesc)

cardPrice :: Card -> Text
cardPrice card = price'
 where
  maybePrice = innerHTML ("strong")
  price'     = fromMaybe "" (extractFrom card maybePrice)

cardDate :: Card -> Text
cardDate card = date'
 where
  maybeDate = innerHTML ("div" @: [hasClass "aditem-addon"])
  date'     = fromMaybe "" (extractFrom card maybeDate)

cardLocation :: Card -> [Text]
cardLocation card = []
 where
  maybePrice = innerHTML ("span" @: [hasClass "cena"])
  price'     = fromMaybe "" (extractFrom card maybePrice)

cardUrl :: Card -> Text
cardUrl card = "https://www.ebay-kleinanzeigen.de" <> url'
 where
  maybeUrl = (attr "href") ("a" @: [hasClass "ellipsis"])
  url'     = fromMaybe "" (extractFrom card maybeUrl)
