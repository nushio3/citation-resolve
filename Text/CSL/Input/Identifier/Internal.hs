{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Text.CSL.Input.Identifier.Internal where

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (toLower)
import           Data.List (span)
import qualified Data.Text as Text
import qualified Data.String.Utils as String (replace) 
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sqlite
import           Network.Curl.Download (openURIWithOpts)
import           Network.Curl.Opts (CurlOption(CurlFollowLocation, CurlHttpHeaders))
import           System.Directory (createDirectoryIfMissing)
import           System.Process (runInteractiveCommand, system)
import           System.IO (hGetContents, hClose)
import           Text.CSL (Reference, readBiblioString, BibFormat(Bibtex, Json))
import           Text.Printf

import qualified Paths_citation_resolve as Paths



-- $setup
-- >>> import Control.Applicative((<$>), (<*>))
-- >>> import Data.Either.Utils(forceEither)
-- >>> import Text.CSL




-- | data structure for accessing the reference cache database.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
WebCache
  url     String
  content BS.ByteString
  deriving Show
|]

-- | 'Resolver' is a function that converts a 'String' key to some
-- value @a@, which may fail with an error message.
type Resolver a = String -> IO (Either String a)

-- | Take a resolver, and make it cached.
cached :: Resolver BS.ByteString -> Resolver BS.ByteString
cached resolver0 url = do
  dbfn <- getDataFileName "reference.db3"

  runResourceT $ withSqlitePool (Text.pack dbfn) 1 $ \pool -> do
      flip runSqlPool pool $ runMigration migrateAll
      mx <- flip runSqlPool pool $ do
        selectFirst [WebCacheUrl ==. url] []
      case mx of
        Just x  -> do
          return $ Right $  webCacheContent $ entityVal x

        Nothing -> do
          ret <- liftIO $ resolver0 url
          case ret of
            Right content0 -> do
              flip runSqlPool pool $ do
                insert $ WebCache
                   url content0
              return ()
            Left _ -> return ()
          return ret




-- | parse a Bibtex entry obtained in various ways.
resolveBibtex :: String -> Resolver Reference
resolveBibtex src str = do
  rs <- readBiblioString Bibtex str
  case rs of
    [r] -> return $ Right r
    []  -> return $ Left $ src ++ " returned no reference."
    _   -> return $ Left $ src ++ " returned multiple references."


-- | Multi-purpose reference ID resolver. Resolve 'String' starting
-- with "arXiv:", "isbn:", "doi:" to 'Reference' .
-- 
-- >>> (==) <$> readArXiv "1204.4779" <*>  readID "arXiv:1204.4779"
-- True
-- >>> (==) <$> readDOI "10.1088/1749-4699/5/1/015003" <*> readID "doi:10.1088/1749-4699/5/1/015003"
-- True
-- >>> (==) <$> readISBN "9780199233212" <*> readID "isbn:9780199233212"
-- True



readID :: Resolver Reference
readID str 
  | idId == "arxiv" = readArXiv addr
  | idId == "doi"   = readDOI addr
  | idId == "isbn"  = readISBN addr
  | otherwise       = return $ Left $ "Unknown identifier type: " ++ str
  where
    (h,t) = span (/=':') str
    idId = map toLower h
    addr = drop 1 t
    



-- | resolve a DOI to a 'Reference'.
--
-- >>> ref <- forceEither <$> readDOI "10.1088/1749-4699/5/1/015003"
-- >>> title ref
-- "Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
-- >>> putStrLn $ url ref
-- http://dx.doi.org/10.1088/1749-4699/5/1/015003



readDOI :: Resolver Reference
readDOI doi = do
  let
      opts = [ CurlFollowLocation True
             , CurlHttpHeaders ["Accept: text/bibliography; style=bibtex"]
             ]
      url = "http://dx.doi.org/" ++ doi
  res <- cached (openURIWithOpts opts) url
  case res of
    Left msg -> return $ Left msg
    Right bs -> resolveBibtex url $ BS.unpack bs

-- | resolve an arXiv ID to a 'Reference'. If it's a referred journal paper, it can also resolve
--   the refereed version of the paper.
-- 
-- >>>  ref <- forceEither <$> readArXiv "1204.4779"
-- >>> title ref
-- "Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
-- >>> containerTitle ref
-- "Computational Science and Discovery"

readArXiv :: Resolver Reference
readArXiv arXiv = do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&arXiv:" ++ arXiv
  res <- cached (openURIWithOpts opts) url
  case res of
    Left msg -> return $ Left msg
    Right bs -> resolveBibtex url $
       String.replace "adsurl" "url" $
       BS.unpack bs

-- | resolve an ISBN to a 'Reference'.
--
-- >>> ref <- forceEither <$> readISBN "9780199233212"
-- >>> title ref
-- "The nature of computation"

readISBN :: Resolver Reference
readISBN isbn = do
  let
      opts = [ CurlFollowLocation True ]
      url = printf "http://xisbn.worldcat.org/webservices/xid/isbn/%s?method=getMetadata&format=xml&fl=*"
            isbn
  res <- cached (openURIWithOpts opts) url
  case res of
    Left msg -> return $ Left msg
    Right bs -> do
      xsltfn <- getDataFileName "isbn2bibtex.xsl"
      writeFile xsltfn xsl
      (hIn,hOut,_,_) <- runInteractiveCommand $ printf "xsltproc %s -" xsltfn
      BS.hPutStr hIn bs
      hClose hIn
      str <- hGetContents hOut
      resolveBibtex url str

  where
    xsl = "<?xml version=\"1.0\"?>\n<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:wc=\"http://worldcat.org/xid/isbn/\" version=\"1.0\">\n    <xsl:output method=\"text\" omit-xml-declaration=\"yes\" indent=\"no\"/>\n    <xsl:template match=\"wc:isbn\">\n        <code>\n    @BOOK{CiteKeyGoesHere,\n        AUTHOR = \"<xsl:value-of select=\"@author\"/>\",\n        TITLE = \"<xsl:value-of select=\"@title\"/>\",\n        PUBLISHER = \"<xsl:value-of select=\"@publisher\"/>\",\n        ADDRESS = \"<xsl:value-of select=\"@city\"/>\",\n        YEAR =\"<xsl:value-of select=\"@year\"/>\"}\n</code>\n    </xsl:template>\n</xsl:stylesheet>\n"


-- | a safer way to get data file name.
getDataFileName :: String -> IO String
getDataFileName fn = do
  dd <- Paths.getDataDir
  createDirectoryIfMissing True dd
  Paths.getDataFileName fn
-- >>> take 7 $ title ref
-- "Paraiso"
