{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Text.CSL.Input.Identifier.Internal where

import           Control.Applicative ((<$>))
import           Control.Lens (_2, Iso, iso, over,  Simple, to, use, (%=))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict as State
import           Control.Monad.Trans.Either
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Generic as AG
import qualified Data.ByteString.Char8 as BS
import           Data.Char (toLower)
import           Data.Default(Default(..))
import           Data.List (span)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.String.Utils as String (replace)
import qualified Data.Yaml as Yaml
import           Network.Curl.Download (openURIWithOpts)
import           Network.Curl.Opts (CurlOption(CurlFollowLocation, CurlHttpHeaders))
import           Safe (headMay)
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


-- | The data structure that carries the resolved references.
newtype DB = DB { unDB :: Map.Map String String}

-- | The lens for accessing the map within the DB.
db :: Simple Iso DB (Map.Map String String)
db = iso unDB DB

instance Default DB where def = DB Map.empty

instance Yaml.FromJSON DB where
  parseJSON x0 = do
    x1 <- Yaml.parseJSON x0
    let x1' :: [(String, [String])]
        x1' = x1
    return $ DB $ Map.fromList $ map (over _2 unlines) x1'

instance Yaml.ToJSON DB where
  toJSON = Yaml.toJSON . map (over _2 lines) . Map.toList . unDB



withDBFile :: (MonadIO m, MonadState DB m) => FilePath -> m a -> EitherT String m a
withDBFile fn prog = do
  con <- liftIO $ BS.readFile fn
  let initStateE :: Either String DB
      initStateE = maybe (Left $ "cannot read/parse DB file: " ++ fn) Right
                         (Yaml.decode con)
  initState <- hoistEither initStateE
  State.put initState
  ret <- lift prog
  finalState <- State.get
  liftIO $ BS.writeFile fn $ Yaml.encode (finalState :: DB)
  return ret


-- | Resolver Monad is a function that converts a key of type @a@ to some
-- other type @b@, which may fail with an error message.
type RM m a b = a -> EitherT String m b

-- | Perform possibly failing IO within a monad

liftIOE :: MonadIO m => IO (Either a b) -> EitherT a m b
liftIOE = (>>= hoistEither) . liftIO

-- | parse a Bibtex entry that contains single reference.
resolveBibtex :: MonadIO m => String -> RM m String Reference
resolveBibtex url str = do
  rs <- liftIO $ readBiblioString Bibtex str
  case rs of
    [r] -> right r
    []  -> left $ url ++ " returned no parsable reference."
    _   -> left $ url ++ " returned multiple references."

-- | Multi-purpose reference ID resolver. Resolve 'String' starting
-- with "arXiv:", "isbn:", "doi:" to 'Reference' .
--
-- >>> (==) <$> readArXiv "1204.4779" <*> readID "arXiv:1204.4779"
-- True
-- >>> (==) <$> readDOI "10.1088/1749-4699/5/1/015003" <*> readID "doi:10.1088/1749-4699/5/1/015003"
-- True
-- >>> (==) <$> readBibcode "2012CS&D....5a5003M" <*>  readID "bibcode:2012CS&D....5a5003M"
-- True
-- >>> (==) <$> readISBN "9780199233212" <*> readID "isbn:9780199233212"
-- True



resolveID :: forall m. (MonadIO m, MonadState DB m) => RM m String Reference
resolveID url = do
  val <- use $ db . to (Map.lookup url)
  case val of
    Just bibtexStr -> resolveBibtex url bibtexStr
    Nothing -> do
      reader <- hoistEither selectResolver
      bibtexStr <- reader addr
      ret <- resolveBibtex url bibtexStr
      db %= Map.insert url bibtexStr
      return ret


   where
     (h,t) = span (/=':') url
     idId = map toLower h
     addr = drop 1 t

     selectResolver :: Either String (RM m String String)
     selectResolver =
       maybe (Left $ "Unknown identifier type: " ++ idId) (Right . snd) $
       headMay $
       filter ((==idId) . fst) specialResolverTbl

     specialResolverTbl =
       [ ("arxiv"   , resolveArXiv   )
       , ("bibcode" , resolveBibcode )
       , ("doi"     , resolveDOI     )
       , ("isbn"    , resolveISBN    ) ]


-- resolvers for specific document IDs.

resolveDOI :: MonadIO m => RM m String String
resolveDOI docIDStr = do
  let
      opts = [ CurlFollowLocation True
             , CurlHttpHeaders ["Accept: text/bibliography; style=bibtex"]
             ]
      url = "http://dx.doi.org/" ++ docIDStr
  bs <- liftIOE $ openURIWithOpts opts url
  return $ BS.unpack bs

resolveArXiv :: MonadIO m => RM m String String
resolveArXiv docIDStr = do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&arXiv:" ++ docIDStr
  bs <- liftIOE $ openURIWithOpts opts url
  return $
    String.replace "adsurl" "url" $
    BS.unpack bs

resolveBibcode :: MonadIO m => RM m String String
resolveBibcode docIDStr = do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&bibcode=" ++ docIDStr
  bs <- liftIOE $ openURIWithOpts opts url
  return $
    String.replace "adsurl" "url" $ BS.unpack bs

resolveISBN :: MonadIO m => RM m String String
resolveISBN docIDStr = do
  let
      opts = [ CurlFollowLocation True ]
      url = printf "http://xisbn.worldcat.org/webservices/xid/isbn/%s?method=getMetadata&format=xml&fl=*"
            docIDStr
  bs <- liftIOE $ openURIWithOpts opts url
  str <- liftIO $ do
    xsltfn <- getDataFileName "isbn2bibtex.xsl"
--    writeFile xsltfn xsl
    (hIn,hOut,_,_) <- runInteractiveCommand $ printf "xsltproc %s -" xsltfn
    BS.hPutStr hIn bs
    hClose hIn
    hGetContents hOut
  return str

  where
    xsl = "<?xml version=\"1.0\"?>\n<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:wc=\"http://worldcat.org/xid/isbn/\" version=\"1.0\">\n    <xsl:output method=\"text\" omit-xml-declaration=\"yes\" indent=\"no\"/>\n    <xsl:template match=\"wc:isbn\">\n        <code>\n    @BOOK{CiteKeyGoesHere,\n        AUTHOR = \"<xsl:value-of select=\"@author\"/>\",\n        TITLE = \"<xsl:value-of select=\"@title\"/>\",\n        PUBLISHER = \"<xsl:value-of select=\"@publisher\"/>\",\n        ADDRESS = \"<xsl:value-of select=\"@city\"/>\",\n        YEAR =\"<xsl:value-of select=\"@year\"/>\"}\n</code>\n    </xsl:template>\n</xsl:stylesheet>\n"


-- | a safer way to get data file name.
getDataFileName :: String -> IO String
getDataFileName fn = do
  dd <- Paths.getDataDir
  createDirectoryIfMissing True dd
  Paths.getDataFileName fn
