module Init where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as JSON
import Data.Aeson.KeyMap (KeyMap, elems)
import RIO (Either (Left, Right), Eq, FilePath, Generic, IO, LByteString, Maybe (Just, Nothing), Show (show), String, Text, fmap, writeFileBinary, ($), (++), (<$>))
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as LB
import RIO.Text (concat)
import Web.Sitemap.Gen (SitemapUrl (SitemapUrl), renderSitemap)
import qualified Web.Sitemap.Gen as Sitemap
import Prelude (putStrLn)

fileName :: FilePath
fileName = "export.json"

outputFileName :: FilePath
outputFileName = "sitemap.xml"

baseUrl :: Text
baseUrl = "https://gil0mendes.io/"

data Page = Page
  { url :: !Text
  , title :: !Text
  }
  deriving (Eq, Show, Generic)

data ExFile = ExFile
  { files :: KeyMap Page
  }
  deriving (Eq, Show, Generic)

instance FromJSON Page where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

instance FromJSON ExFile where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

getExportedContent :: IO LByteString
getExportedContent = LB.readFile fileName

parseData :: IO (Either String ExFile)
parseData = do
  JSON.eitherDecode <$> getExportedContent

generateUrl :: Page -> Text
generateUrl Page {url} = RIO.Text.concat [baseUrl, url, ".html"]

fromPage :: Page -> SitemapUrl
fromPage page =
  SitemapUrl
    { sitemapLocation = generateUrl page
    , sitemapLastModified = Nothing
    , sitemapChangeFrequency = Just Sitemap.Monthly
    , sitemapPriority = Nothing
    }

generateSitemapXml :: [Page] -> B.ByteString
generateSitemapXml pages = renderSitemap sitemap
  where
    sitemap = Sitemap.Sitemap $ fmap fromPage pages

writeXmlFile :: B.ByteString -> IO ()
writeXmlFile = writeFileBinary outputFileName

runApp :: IO ()
runApp = do
  parsedData <- parseData
  case parsedData of
    (Left error) -> do
      putStrLn $ "Impossible to parse the given JSON: " ++ error
    (Right filesMap) ->
      let listOfFiles = elems $ files filesMap
          renderedXml = generateSitemapXml listOfFiles
       in writeXmlFile renderedXml
