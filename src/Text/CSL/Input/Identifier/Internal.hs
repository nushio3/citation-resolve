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
import qualified Data.ByteString.Char8 as BS
import           Data.Char (toLower)
import           Data.List (span)
import qualified Data.Text as Text
import qualified Data.String.Utils as String (replace)
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



-- | 'Resolver' is a function that converts a 'String' key to some
-- value @a@, which may fail with an error message.
type Resolver a = String -> IO (Either String a)

-- | Take a resolver, and make it cached.
cached :: String -> Resolver Reference -> Resolver Reference
cached salt resolver0 = resolver0

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
-- >>> (==) <$> readBibcode "2012CS&D....5a5003M" <*>  readID "bibcode:2012CS&D....5a5003M"
-- True
-- >>> (==) <$> readISBN "9780199233212" <*> readID "isbn:9780199233212"
-- True



readID :: Resolver Reference
readID str
  | idId == "arxiv"   = readArXiv addr
  | idId == "bibcode" = readBibcode addr
  | idId == "doi"     = readDOI addr
  | idId == "isbn"    = readISBN addr
  | otherwise         = return $ Left $ "Unknown identifier type: " ++ str
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
readDOI = cached "doi" $ \docIDStr -> do
  let
      opts = [ CurlFollowLocation True
             , CurlHttpHeaders ["Accept: text/bibliography; style=bibtex"]
             ]
      url = "http://dx.doi.org/" ++ docIDStr
  res <- openURIWithOpts opts url
  case res of
    Left msg -> return $ Left $ url ++ " : " ++ msg
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
readArXiv = cached "arXiv" $ \docIDStr -> do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&arXiv:" ++ docIDStr
  res <- openURIWithOpts opts url
  case res of
    Left msg -> return $ Left msg
    Right bs -> resolveBibtex url $
       String.replace "adsurl" "url" $
       BS.unpack bs



-- | resolve an Bibcode ID to a 'Reference'.
--
-- >>>  ref <- forceEither <$> readBibcode " 2012CS&D....5a5003M"
-- >>> title ref
-- "Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
-- >>> containerTitle ref
-- "Computational Science and Discovery"

readBibcode :: Resolver Reference
readBibcode = cached "bibcode" $ \docIDStr -> do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&bibcode=" ++ docIDStr
  res <- openURIWithOpts opts url
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
readISBN = cached "ISBN" $ \docIDStr -> do
  let
      opts = [ CurlFollowLocation True ]
      url = printf "http://xisbn.worldcat.org/webservices/xid/isbn/%s?method=getMetadata&format=xml&fl=*"
            docIDStr
  res <- openURIWithOpts opts url
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
