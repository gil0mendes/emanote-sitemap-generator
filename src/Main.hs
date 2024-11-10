{-# LANGUAGE NamedFieldPuns #-}

module Init where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap (KeyMap, elems)
import RIO (Either (Left, Right), Eq, FilePath, Generic, IO, LByteString, Maybe (Just, Nothing), String, Text, fmap, writeFileBinary, ($), (++), (<$>))
import RIO.ByteString qualified as B
import RIO.ByteString.Lazy qualified as LB
import RIO.Text (concat)
import Web.Sitemap.Gen (SitemapUrl (SitemapUrl), renderSitemap, sitemapChangeFrequency, sitemapLastModified, sitemapLocation, sitemapPriority)
import Web.Sitemap.Gen qualified as Sitemap
import Prelude (putStrLn)

fileName :: RIO.FilePath
fileName = "export.json"

outputFileName :: RIO.FilePath
outputFileName = "sitemap.xml"

baseUrl :: RIO.Text
baseUrl = "https://gil0mendes.io/"

data Page = Page
  { url :: !RIO.Text
  , title :: !RIO.Text
  }
  deriving (RIO.Eq, RIO.Show, RIO.Generic)

data ExFile = ExFile
  { files :: KeyMap Page
  }
  deriving (RIO.Eq, RIO.Show, RIO.Generic)

instance FromJSON Page where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

instance FromJSON ExFile where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

getExportedContent :: RIO.IO RIO.LByteString
getExportedContent = LB.readFile fileName

parseData :: RIO.IO (RIO.Either RIO.String ExFile)
parseData = do
  JSON.eitherDecode RIO.<$> getExportedContent

generateUrl :: Page -> RIO.Text
generateUrl Page{url} = RIO.Text.concat [baseUrl, url, ".html"]

fromPage :: Page -> SitemapUrl
fromPage page =
  SitemapUrl
    { sitemapLocation = generateUrl page
    , sitemapLastModified = RIO.Nothing
    , sitemapChangeFrequency = RIO.Just Sitemap.Monthly
    , sitemapPriority = RIO.Nothing
    }

generateSitemapXml :: [Page] -> B.ByteString
generateSitemapXml pages = renderSitemap sitemap
 where
  sitemap = Sitemap.Sitemap RIO.$ RIO.fmap fromPage pages

writeXmlFile :: B.ByteString -> RIO.IO ()
writeXmlFile = RIO.writeFileBinary outputFileName

runApp :: RIO.IO ()
runApp = do
  parsedData <- parseData
  case parsedData of
    (RIO.Left error) -> do
      putStrLn RIO.$ "Impossible to parse the given JSON: " RIO.++ error
    (RIO.Right filesMap) ->
      let listOfFiles = elems RIO.$ files filesMap
          renderedXml = generateSitemapXml listOfFiles
       in writeXmlFile renderedXml

main :: RIO.IO ()
main = runApp
