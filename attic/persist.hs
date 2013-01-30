{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
       
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
PersistentReference
  identifier String
  referenceBibtex String
|]



main :: IO ()
main = do
  runResourceT $ withSqlitePool "mytest.db3" 10 $
    \pool -> do
      flip runSqlPool pool $ runMigration migrateAll
      flip runSqlPool pool $ do
        insert $ PersistentReference
          "arXiv:1234.5678"
          "On the persistent management of the reference"
      return ()

  runResourceT $ withSqlitePool "mytest.db3" 10 $
    \pool -> do
      flip runSqlPool pool $ do
        insert $ PersistentReference
          "arXiv:1234.7171"
          "Yorualogistics"
      return ()