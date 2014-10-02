{-# LANGUAGE FlexibleContexts #-}
-- | This modules provides a way to convert document identifiers, such
--  as DOIs, ISBNs, arXiv IDs to bibliographic references.
--
--  Each type of identifiers will be converted via internet services
--  to a bibliographic record of type 'Text.CSL.Reference' , which in
--  turn can be rendered in various format using @citeproc-hs@ package
--  <hackage.haskell.org/package/citeproc-hs> .
--
--  Moreover, the server responses are cached in a local database,
--  making the server load as little as possible.

module Text.CSL.Input.Identifier
       (resolveEither, resolve, withDatabaseFile, Database(..), database, databaseMap, HasDatabase(..),
        resolveDef, resolveEitherDef, 
        toBibTeXItem)
       where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class
import           Control.Monad.State as State
import           Control.Monad.Trans.Either
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Text.CSL.Reference (emptyReference, Reference)
import           Text.CSL.Input.Identifier.Internal

-- $setup
-- >>> import Control.Applicative((<$>), (<*>))
-- >>> import Data.Either.Utils(forceEither)
-- >>> import Text.CSL



-- | Resolve a document url to a 'Reference'. returns an empty reference when someting fails.
--   prefix the document ID with one of "arXiv:", "doi:", "bibcode:" or "isbn:" .
--
--
-- >>> do { ref <- resolveDef "arXiv:1204.4779" ; putStrLn $ title ref }
-- Paraiso: an automated tuning framework for explicit solvers of partial differential equations
-- >>> do { ref <- resolveDef "doi:10.1088/1749-4699/5/1/015003" ; print $ author ref }
-- [Takayuki Muranushi]
-- >>> do { ref <- resolveDef "bibcode:2012CS&D....5a5003M" ; putStrLn $ containerTitle ref }
-- Computational Science and Discovery
-- >>> do { ref <- resolveDef "isbn:9784274068850" ; putStrLn $ title ref }
-- Sugoi hasukeru tanoshiku manabō


resolve :: (MonadIO m, MonadState s m, HasDatabase s) => String -> m Reference
resolve = liftM (either (const emptyReference) id) . runEitherT . resolveEither


-- | Access the resolver database and generate the BibTeX item string for the document,
--   using the url as the citation-key.

toBibTeXItem :: (MonadIO m, MonadState s m, HasDatabase s) => String -> m Text.Text
toBibTeXItem url = do 
  _ <- resolve url
  dbMap <- Lens.use databaseMap
  let ret = case Map.lookup url dbMap of
        Nothing -> ""
        Just str -> let (s0,s1) = break (=='{') str
                        (_,s2) = break (==',') s1
                    in s0 ++ "{" ++ url ++ s2
          
  return $ Text.pack ret


-- | Resolve the document id using the default database.

resolveDef :: String -> IO Reference
resolveDef url = do
  fn <- getDataFileName "default.db"
  let go = withDatabaseFile fn $ resolve url
  State.evalStateT go (def :: Database)

-- | Resolve the document id using the default database, return an either value with 
--   any errors that occur

resolveEitherDef :: String -> IO (Either String Reference)
resolveEitherDef s = do
  fn <- getDataFileName "default.db"
  let go = withDatabaseFile fn $ ( (runEitherT.resolveEither) s)
  State.evalStateT go (Database Map.empty)
