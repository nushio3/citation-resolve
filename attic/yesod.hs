{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/person/#PersonId PersonR GET
|]

-- Nothing special here
instance Yesod PersistTest

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlPersist

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.
getPersonR :: PersonId -> Handler RepPlain
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ RepPlain $ toContent $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runResourceT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runSqlPool (insert $ Person "Michael" "Snoyman" 26) pool
    liftIO $ warpDebug 3000 $ PersistTest pool
