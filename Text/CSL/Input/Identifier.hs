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
       (EReference, readID, readDOI, readArXiv, readBibcode, readISBN )
       where


import qualified Text.CSL
import           Text.CSL.Input.Identifier.Internal


-- | EReference is the type returned by the 'Reference' resolver,
-- accompanied with possible error message.

type EReference = Either String Text.CSL.Reference
