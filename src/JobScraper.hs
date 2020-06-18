{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module JobScraper where

import           Data.Aeson
import           Data.ByteString               as BS
                                                ( ByteString )
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

data JobAd = JobAd {  title        :: Text
                    , keywords     :: [Text]
                    , companyName  :: Text
                    , entryDate    :: Text
                    , url          :: Text} deriving (Generic, ToJSON, FromJSON, Show)

extractFrom = scrapeStringLike

getJobsAdList :: IO [JobAd]
getJobsAdList = do
  jobs <- getJobsBS
  let decodedJobs =
        fromMaybe [] ((decodeStrict jobs) :: Maybe [JobAd])
  return decodedJobs

getJobsBS :: IO (BS.ByteString)
getJobsBS = do
  maybeCards <- scrapeJobCards
  let cards = fromMaybe [] maybeCards

  let ads = map
        (\card -> JobAd { title       = (cardTitle card)
                        , keywords    = (cardKeywords card)
                        , companyName = (cardCompanyName card)
                        , entryDate   = (cardEntryDate card)
                        , url         = (cardUrl card)
                        }
        )
        (if length cards > 5 then take 5 cards else cards)
  let adsStr = encode ads
  pure $ toStrict adsStr

getJobPage :: IO Text
getJobPage = runReq defaultHttpConfig $ do
  response <- req GET
                  (https "slo-tech.com" /: "delo")
                  (NoReqBody)
                  bsResponse $
                  mempty 
  pure $ decodeUtf8 $ responseBody response

scrapeJobCards :: IO (Maybe [Card])
scrapeJobCards = do
  let scraper = htmls $ ("table" @: [hasClass "forums"] // "tbody" // "tr")
  page <- getJobPage
  let jobCards' = extractFrom page scraper
  pure jobCards'

cardUrl :: Card -> Text
cardUrl card = "https://slo-tech.com" <> url'
 where
  maybeUrl = attr "href" $ ("td" @: [hasClass "name"] // "a")
  url'     = fromMaybe "" (extractFrom card maybeUrl)

cardTitle :: Card -> Text
cardTitle card = title'
 where
  maybeTitle = innerHTML $ ("td" @: [hasClass "name"] // "a")
  title'     = fromMaybe "" (extractFrom card maybeTitle)

cardKeywords :: Card -> [Text]
cardKeywords card = keywords'
 where
  maybeKeywords = innerHTMLs $ ("span" @: [hasClass "oddelek"] // "a")
  keywords'     = fromMaybe [] (extractFrom card maybeKeywords)

cardCompanyName :: Card -> Text
cardCompanyName card = companyName'
 where
  maybeCompanyName = innerHTML $ ("td" @: [hasClass "company"] // "a")
  companyName'     = fromMaybe "" (extractFrom card maybeCompanyName)

cardEntryDate :: Card -> Text
cardEntryDate card = entryDate'
 where
  maybeEntryDate = attr "datetime" $ ("td" @: [hasClass "last_msg"] // "time")
  entryDate'     = fromMaybe "" (extractFrom card maybeEntryDate)


