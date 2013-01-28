module Text.CSL.Input.Identifier where


import           Control.Applicative ((<$>))
import           Network.Curl.Download (openURIWithOpts)
import           Network.Curl.Opts (CurlOption(CurlFollowLocation, CurlHttpHeaders))
import qualified Data.ByteString.Char8 as BS
import           Text.CSL (Reference, readBiblioString, BibFormat(Bibtex, Json))
import           Text.Printf
import           Data.String.Utils (replace)

-- $setup
-- >>> import Text.CSL

-- | resolve a DOI to a 'Reference'.
--
-- > (\(Right x) -> take 7 $ title x) <$> readDOI "10.1088/1749-4699/5/1/015003"
--
-- "Paraiso"

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
-- > (\(Right x) -> take 7 $ title x) <$> readArXiv "1204.
--
-- "Paraiso"


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
-- >>> readIsbn "0521288843"
-- "Paraiso"


readIsbn :: String -> IO (Either String Reference)
readIsbn isbn = do
  let
      opts = [ CurlFollowLocation True]
      url = printf "http://xisbn.worldcat.org/webservices/xid/isbn/%s?method=getMetadata&format=json&fl=*"
            isbn
  res <- openURIWithOpts opts url
  case res of
    Left msg -> return $ Left msg
    Right bs -> do
      rs <- readBiblioString Json $
            replace "\"stat\":\"ok\"," "" $
            BS.unpack bs
      case rs of
        [r] -> return $ Right r
        []  -> return $ Left $ url ++ " returned no reference."
        _   -> do
          print rs
          return $ Left $ url ++ " returned multiple references."
