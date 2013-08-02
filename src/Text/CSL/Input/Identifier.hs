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
       (resolveEither, resolve)
       where

import           Control.Monad.IO.Class
import           Control.Monad.State as State
import           Control.Monad.Trans.Either
import           Data.Default
import           Text.CSL.Reference (emptyReference, Reference)
import           Text.CSL.Input.Identifier.Internal

-- $setup
-- >>> import Control.Applicative((<$>), (<*>))
-- >>> import Data.Either.Utils(forceEither)
-- >>> import Text.CSL



-- | resolve a document url to a 'Reference'. returns an empty reference when someting fails. 
--  "arXiv:", "isbn:", "doi:" to 'Reference' .
-- 
--
-- >>> liftM title $ resolveDef "arXiv:1204.4779"
-- "Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
-- >>> liftM title $ resolveDef "doi:10.1088/1749-4699/5/1/015003"
-- "Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
-- >>> liftM title $ resolveDef "bibcode:2012CS&D....5a5003M"
-- "Paraiso: an automated tuning framework for explicit solvers of partial differential equations"
-- >>> liftM title $ resolveDef "isbn:9780199233212"
-- "The nature of computation"


resolve :: (MonadIO m, MonadState DB m) => String -> m Reference
resolve = liftM (either (const emptyReference) id) . runEitherT . resolveEither 

-- | Resolve the document id using the default database.

resolveDef :: String -> IO Reference
resolveDef url = do
  fn <- getDataFileName "default.db"             
  let go = withDBFile fn $ resolve url
  State.evalStateT go def
