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
import           Control.Lens (_2, Iso, iso, over,  Simple, to, use, (%=), (.=))
import           Control.Lens.TH 
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State as State
import           Control.Monad.Trans.Either
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Generic as AG
import qualified Data.ByteString.Char8 as BS
import           Data.Char (toLower, isSpace)
import           Data.Default(Default(..))
import           Data.List (span)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.String.Utils as String (replace)
import qualified Data.Yaml as Yaml
import           Network.Curl.Download (openURIWithOpts)
import           Network.Curl.Opts (CurlOption(CurlFollowLocation, CurlHttpHeaders))
import           Safe (headMay)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Process (runInteractiveCommand, system)
import           System.IO (hGetContents, hClose, hPutStrLn, stderr)
import           Text.CSL (Reference, readBiblioString, BibFormat(Bibtex, Json))
import           Text.Printf

import qualified Paths_citation_resolve as Paths



-- | The data structure that carries the resolved references.  Since
--   the mapping @Reference -> BibTeX@ is not the inverse of 
--   @BibTeX -> Reference@ for the version @citeproc-hs-0.3.8@ and loses some
--   information, we choose to store the original BibTeX string in the Database,
--  rather  than 'Reference'.
newtype Database = Database { _databaseMap :: Map.Map String String}

-- | The lens for accessing the map within the Database.

makeClassy ''Database
--db :: Simple Iso Database (Map.Map String String)
--db = iso unDatabase Database

instance Default Database where def = Database Map.empty

instance Yaml.FromJSON Database where
  parseJSON x0 = do
    x1 <- Yaml.parseJSON x0
    let x1' :: [(String, [String])]
        x1' = x1
    return $ Database $ Map.fromList $ map (over _2 unlines) x1'

instance Yaml.ToJSON Database where
  toJSON = Yaml.toJSON . map (over _2 lines) . Map.toList . _databaseMap




withDatabaseFile :: (MonadIO m, MonadState s m, HasDatabase s) => FilePath -> m a -> m a
withDatabaseFile fn prog = do
  x <- liftIO $ doesFileExist fn
  initState <- case x of
    False -> return def
    True -> do
      con <- liftIO $ BS.readFile fn
      case Yaml.decode con of
        Just st -> return st
        Nothing -> do
          liftIO $ hPutStrLn stderr $ "cannot read/parse Database file: " ++ fn
          return def
  database .= (initState::Database)
  ret <- prog
  finalState <- use database
  liftIO $ BS.writeFile fn $ Yaml.encode (finalState :: Database)
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



-- | resolve a document url to a 'Reference', or emits a error
--   message with reason why it fails.
resolveEither :: forall m s.(MonadIO m, MonadState s m, HasDatabase s) => String -> EitherT String m Reference
resolveEither url = do
  val <- use $ databaseMap . to (Map.lookup url)
  case val of
    Just bibtexStr -> resolveBibtex url bibtexStr
    Nothing -> do
      reader <- hoistEither selectResolver
      bibtexStr <- reader addr
      ret <- resolveBibtex url bibtexStr
      databaseMap %= Map.insert url bibtexStr
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
    unlines . drop 2 . filter (any (not . isSpace)) . lines $
    String.replace "adsurl" "url" $
    BS.unpack bs

resolveBibcode :: MonadIO m => RM m String String
resolveBibcode docIDStr = do
  let
      opts = [ CurlFollowLocation True]
      url = "http://adsabs.harvard.edu/cgi-bin/bib_query?data_type=BIBTEX&bibcode=" ++ docIDStr
  bs <- liftIOE $ openURIWithOpts opts url
  return $
    unlines . drop 2 . filter (any (not . isSpace)) . lines $
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
    writeFile xsltfn xsl
    (hIn,hOut,_,_) <- runInteractiveCommand $ printf "xsltproc %s -" xsltfn
    BS.hPutStr hIn bs
    hClose hIn
    hGetContents hOut
  return $ unlines . filter (any (not . isSpace)) . lines $ str

  where
    -- we must dynamically generate this file because it does not
    -- exist at the test timing.

    xsl = "<?xml version=\"1.0\"?>\n<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" xmlns:wc=\"http://worldcat.org/xid/isbn/\" version=\"1.0\">\n    <xsl:output method=\"text\" omit-xml-declaration=\"yes\" indent=\"no\"/>\n    <xsl:template match=\"wc:isbn\">\n        <code>\n    @BOOK{CiteKeyGoesHere,\n        AUTHOR = \"<xsl:value-of select=\"@author\"/>\",\n        TITLE = \"<xsl:value-of select=\"@title\"/>\",\n        PUBLISHER = \"<xsl:value-of select=\"@publisher\"/>\",\n        ADDRESS = \"<xsl:value-of select=\"@city\"/>\",\n        YEAR =\"<xsl:value-of select=\"@year\"/>\"}\n</code>\n    </xsl:template>\n</xsl:stylesheet>\n"


-- | a safer way to get data file name.
getDataFileName :: String -> IO String
getDataFileName fn = do
  dd <- Paths.getDataDir
  createDirectoryIfMissing True dd
  Paths.getDataFileName fn
