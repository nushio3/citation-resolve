module Text.CSL.Input.Identifier where


import           Control.Applicative ((<$>))
import           Network.Curl.Download (openURIWithOpts)
import           Network.Curl.Opts (CurlOption(CurlFollowLocation, CurlHttpHeaders))
import qualified Data.ByteString.Char8 as BS
import           Text.CSL (Reference, readBiblioString, BibFormat(Bibtex, Json))
import           Text.Printf
import           System.Process (runInteractiveCommand)
import           System.IO (hGetContents, hClose)

-- $setup
-- >>> import Text.CSL

-- | resolve a DOI to a 'Reference'.
--
-- >>> (fmap (take 7 . title)) <$>  readDOI "10.1088/1749-4699/5/1/015003"
-- Right "Paraiso"

readDOI :: String -> IO (Either String Reference)
readDOI doi = do
  let
      opts = [ CurlFollowLocation True
             , CurlHttpHeaders ["Accept: text/bibliography; style=bibtex"]
             ]
      url = "http://dx.doi.org/" ++ doi
  res <- openURIWithOpts opts url
  case res of
    Left msg -> return $ Left msg
    Right bs -> do
      rs <- readBiblioString Bibtex $ BS.unpack bs
      case rs of
        [r] -> return $ Right r
        []  -> return $ Left $ url ++ " returned no reference."
        _   -> return $ Left $ url ++ " returned multiple references."

-- | resolve an arXiv ID to a 'Reference'.
--
-- >>> (fmap (take 7 . title)) <$> readArXiv "1204.4779"
-- Right "Paraiso"


readArXiv :: String -> IO (Either String Reference)
readArXiv arXiv = do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&arXiv:" ++ arXiv
  res <- openURIWithOpts opts url
  case res of
    Left msg -> return $ Left msg
    Right bs -> do
      rs <- readBiblioString Bibtex $ BS.unpack bs
      case rs of
        [r] -> return $ Right r
        []  -> return $ Left $ url ++ " returned no reference."
        _   -> return $ Left $ url ++ " returned multiple references."

-- | resolve an ISBN to a 'Reference'.
--
-- >>> (fmap title) <$> readIsbn "9780199233212"
-- Right "The nature of computation"


readIsbn :: String -> IO (Either String Reference)
readIsbn isbn = do
  let
      opts = [ CurlFollowLocation True ]
      url = printf "http://xisbn.worldcat.org/webservices/xid/isbn/%s?method=getMetadata&format=xml&fl=*"
            isbn
  res <- openURIWithOpts opts url
  case res of
    Left msg -> return $ Left msg
    Right bs -> do
      let xsltfn = "/tmp/isbn2bibtex.xsl"
      writeFile xsltfn xsl
      (hIn,hOut,_,_) <- runInteractiveCommand $ printf "xsltproc %s -" xsltfn
      BS.hPutStr hIn bs
      hClose hIn
      str <- hGetContents hOut
      rs <- readBiblioString Bibtex str

      case rs of
        [r] -> return $ Right r
        []  -> return $ Left $ url ++ " returned no reference."
        _   -> do
          print rs
          return $ Left $ url ++ " returned multiple references."

  where
    xsl = "<?xml version=\"1.0\"?>\n<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:wc=\"http://worldcat.org/xid/isbn/\" version=\"1.0\">\n    <xsl:output method=\"text\" omit-xml-declaration=\"yes\" indent=\"no\"/>\n    <xsl:template match=\"wc:isbn\">\n        <code>\n    @BOOK{CiteKeyGoesHere,\n        AUTHOR = \"<xsl:value-of select=\"@author\"/>\",\n        TITLE = \"<xsl:value-of select=\"@title\"/>\",\n        PUBLISHER = \"<xsl:value-of select=\"@publisher\"/>\",\n        ADDRESS = \"<xsl:value-of select=\"@city\"/>\",\n        YEAR =\"<xsl:value-of select=\"@year\"/>\"}\n</code>\n    </xsl:template>\n</xsl:stylesheet>\n"
