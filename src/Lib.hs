{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Prelude                 hiding ( id )
import           Aws.Lambda
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import Data.Char (toLower)
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text)
import           GHC.Generics                   ( Generic )
import           ApartmentScraper               ( getApartmentsAdList, ApartmentAd(..) )
import           JobScraper                     ( getJobsAdList, JobAd(..) )
import           EbayKScraper                   ( getEbayKAds, EbayKAd(..) )
import           ShoppingTool
import           Network.HTTP.Req

-- TELEGRAM API TOKEN
-- Fill out before running
-- botToken = ""

-- Input
data MessageEvent = MessageEvent
                   { update_id  :: Int
                   , message :: Message } deriving (Generic, FromJSON, Show)

data Message = Message
                   { message_id :: Int
                   , from :: MessageMetadata
                   , text :: Text } deriving (Generic, FromJSON, Show)

data MessageMetadata = MessageMetadata
                   { id :: Int
                   , is_bot :: Bool
                   , username :: Text } deriving (Generic, FromJSON, Show)
-- Output
data Response = Response
                   { statusCode :: Int
                   , body :: String
                   } deriving (Generic, ToJSON)

handler :: MessageEvent -> Context -> IO (Either String Response)
handler MessageEvent {..} context = do
  let chatid = id $ from message
  let messageContent = text message
  let splitMessage = Text.words messageContent
  let mainCommand = head splitMessage
  let arguments = tail splitMessage
  case splitMessage of
    ["/start"] -> mapM
      (\t -> sendMessage t chatid)
      [ "Meow! I offer various utilities."
      , "Currently available commands:"
      , "<code>/jobs</code> - Print current job offers"
      , "<code>/price [item]</code> - Look up the price of an item in local stores"
      , "<code>/ebayk [item]</code> - Search eBay-Kleinanzeigen for an item"
      ]
    ["/apartments"] -> do
      sendMessage "Current apartments matching the filter:" chatid
      apartments <- getApartmentsAdList
      mapM (\apartment -> sendMessage (styleApartment apartment) chatid) apartments 
    ["/jobs"] -> do
      sendMessage "Latest jobs on Slo-Tech:" chatid
      jobs <- getJobsAdList
      mapM (\job -> sendMessage (styleJob job) chatid) jobs
    multiWordQuery -> do
      if mainCommand == "/ebayk" then do
        sendMessage ("Latest '" <> (Text.unwords arguments) <> "' offers:") chatid
        ebaykads <- getEbayKAds arguments
        let decodedEbayKAds =
              fromMaybe [] ((decodeStrict ebaykads) :: Maybe [EbayKAd])
        mapM (\ebaykad -> sendMessage (styleEbayKAd ebaykad) chatid) decodedEbayKAds
      else if mainCommand == "/price" then do
        let itemName = Text.toLower $ Text.unwords arguments
        itemList <- getShoppingItemList
        let maybeShoppingItem = getShoppingItem itemName itemList
        case maybeShoppingItem of
          Nothing -> mapM (\t -> sendMessage t chatid) ["ShoppingItem not found."]
          Just item -> mapM (\t -> sendMessage (styleShoppingItemPrices t) chatid) [item]
      else mapM (\t -> sendMessage t chatid) ["Unknown command."]
  pure $ Right Response { statusCode = 200, body = "handler ok" }

sendMessage :: Text -> Int -> IO ()
sendMessage text chatid = runReq defaultHttpConfig $ do
  let payload = object
        [ "text" .= text
        , "chat_id" .= ((show chatid) :: String)
        , "parse_mode" .= ("html" :: String)
        , "disable_web_page_preview" .= ("True" :: String)
        ]
  r <- req
    POST
    (  https "api.telegram.org"
    /: botToken
    /: "sendMessage"
    ) 
    (ReqBodyJson payload) 
    jsonResponse $
    header "User-Agent" "AdsBot-Google (+http://www.google.com/adsbot.html)"
  liftIO $ print (responseBody r :: Value)

divider = "----------"

styleJob :: JobAd -> Text
styleJob JobAd {..} = html
 where
  html = Text.unlines
    [ divider
    , bold title
    , bold companyName
    , Text.unlines $ italics <$> keywords
    , italics entryDate
    , url
    ]

styleApartment :: ApartmentAd -> Text
styleApartment ApartmentAd {..} = html
 where
  html =
    Text.unlines [divider, bold region, bold size, bold price, desc, url]

styleEbayKAd :: EbayKAd -> Text
styleEbayKAd EbayKAd {..} = html
  where
    html = Text.unlines [italics date, bold title, desc, bold price, url]

styleShoppingItemPrices :: ShoppingItem -> Text
styleShoppingItemPrices ShoppingItem {..} = html
  where
    unit = " EUR / kg"
    html = Text.unlines $ Text.pack <$> [ (if hoferPrice /= 0     then ("Hofer: "     <> (show hoferPrice)      <> unit) else "")
                                        , (if tusPrice /= 0       then ("Tus: "       <> (show tusPrice)        <> unit) else "")
                                        , (if sparPrice /= 0      then ("Spar: "      <> (show sparPrice)       <> unit) else "")
                                        , (if mercatorPrice /= 0  then ("Mercator: "  <> (show mercatorPrice)   <> unit) else "")]

bold :: Text -> Text
bold str = "<b>" <> str <> "</b>"

italics :: Text -> Text
italics str = "<i>" <> str <> "</i>"
