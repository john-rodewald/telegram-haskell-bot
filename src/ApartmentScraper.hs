{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ApartmentScraper where

import           Data.Aeson
import           Data.ByteString               as BS
                                                ( ByteString )
import           Control.Monad.IO.Class         ( liftIO )

import           Data.ByteString.Lazy          as BSL
                                                ( toStrict )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as Text
                                                ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           GHC.Generics
import           Text.HTML.Scalpel
import           Network.HTTP.Req


type Card = Text

data ApartmentAd = ApartmentAd { region :: Text
                               , size   :: Text
                               , price  :: Text
                               , desc   :: Text
                               , url  :: Text} deriving (Generic, ToJSON, FromJSON, Show)

extractFrom = scrapeStringLike

getApartmentsAdList :: IO [ApartmentAd]
getApartmentsAdList = do
  apartments <- getApartmentsBS
  let decodedApartments = fromMaybe [] ((decodeStrict apartments) :: Maybe [ApartmentAd])
  return decodedApartments

getApartmentsBS :: IO (BS.ByteString)
getApartmentsBS = do
  maybeCards <- scrapeApartmentCards
  let cards = fromMaybe [] maybeCards

  let ads = map
        (\card -> ApartmentAd { region = (cardRegion card)
                              , size   = (cardSize card)
                              , price  = (cardPrice card)
                              , desc   = (cardDesc card)
                              , url  = (cardUrl card)
                              }
        )
        (take 5 cards) 
  let adsStr = encode ads
  pure $ toStrict adsStr

getApartmentPage :: IO Text
getApartmentPage = runReq defaultHttpConfig $ do
  response <- req
    GET -- method
    (  https "nepremicnine.net"
    /: "oglasi-oddaja"
    /: "ljubljana-mesto"
    /: "stanovanje"
    ) 
    (NoReqBody)
    bsResponse $ 
    header "User-Agent" "AdsBot-Google (+http://www.google.com/adsbot.html)"
  pure $ decodeUtf8 $ responseBody response

scrapeApartmentCards :: IO (Maybe [Card])
scrapeApartmentCards = do
  let scraper = htmls ("div" @: [hasClass "oglas_container"])
  page <- getApartmentPage
  let apartmentCards' = extractFrom page scraper
  pure apartmentCards'

cardUrl :: Card -> Text
cardUrl card = "https://www.nepremicnine.net" <> url'
 where
  maybeUrl = (attr "data-href") ("h2")
  url'     = fromMaybe "" (extractFrom card maybeUrl)

cardRegion :: Card -> Text
cardRegion card = region'
 where
  maybeRegion = innerHTML ("span" @: [hasClass "title"])
  region'     = fromMaybe "" (extractFrom card maybeRegion)

cardSize :: Card -> Text
cardSize card = size'
 where
  maybeSize = innerHTML ("span" @: [hasClass "velikost"])
  size'     = fromMaybe "" (extractFrom card maybeSize)

cardDesc :: Card -> Text
cardDesc card = desc'
 where
  maybeDesc = innerHTML ("div" @: [hasClass "kratek"])
  desc'     = fromMaybe "" (extractFrom card maybeDesc)

cardPrice :: Card -> Text
cardPrice card = price'
 where
  maybePrice = innerHTML ("span" @: [hasClass "cena"])
  price'     = fromMaybe "" (extractFrom card maybePrice)
