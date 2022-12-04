{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Network.Socket
import Network.URI
import Network.HTTP.Client
import Text.HTML.Scalpel
import Data.Time
import Control.Monad (forM_, forever, when)
import Data.Maybe
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.List (group, sort, isInfixOf)
import Control.Arrow ((&&&))

import System.Command
import System.Exit

import Control.Applicative
import Options hiding (group)

data MainOptions = MainOptions
    { optSeed :: String
    , optExport :: Bool
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "seed" ""
            "seed url to start"
        <*> simpleOption "export" False
            "export data"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Page 
    from String
    to String
    scraped Bool
    timestamp UTCTime
    UniquePage to
|]

getBody :: String -> IO Stdout
getBody url = cmd $ "curl --socks5-hostname localhost:9150 " <> url

getHrefs :: String -> [String]
getHrefs body = maybe [] id $ scrapeStringLike body (attrs "href" "a")

parseUriWithParent :: String -> String -> Maybe String
parseUriWithParent parent child = do 
    parentUri <- parseURIReference parent
    childUri <- parseURIReference child
    if uriIsRelative childUri 
        then return $ show $ relativeTo childUri parentUri 
        else return $ show childUri

getPageDetails :: String -> IO [String]
getPageDetails url = do 
    Stdout out <- getBody url
    return $ filter isOnionUrl $ mapMaybe (parseUriWithParent url) (getHrefs out)

isOnionUrl :: String -> Bool
isOnionUrl url = maybe False id $ do 
    uri <- parseURIReference url
    uriAuth <- uriAuthority uri
    return $ isInfixOf ".onion" (uriRegName uriAuth)

urlHost :: String -> Maybe String
urlHost url = do 
    uri <- parseURIReference url
    auth <- uriAuthority uri
    return $ uriRegName auth

pagesPairs :: Entity Page -> Maybe (String,String)
pagesPairs (Entity _ Page{..}) = do
    fromHost <- urlHost pageFrom
    toHost <- urlHost pageTo
    return (fromHost, toHost)

countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences = map (head &&& length) . group . sort

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "data.db3" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll

    runCommand $ \opts args -> do
        when (optExport opts) $ do
            putStrLn "Exporting data.."
            scrapedPages <- flip runSqlPool pool $ selectList [ PageScraped ==. True ] []
            let result = countOccurences $ mapMaybe pagesPairs scrapedPages
                exportContent = unlines $ fmap (\((from,to),count)-> from <> "," <> to <> "," <> (show count)) result
            writeFile "data.csv" exportContent


        when (not (optExport opts)) $ do
            now <- getCurrentTime
            when ((optSeed opts) /= "") $ do
                putStrLn "Deleting all pages not scraped from database"
                flip runSqlPool pool $ do 
                    deleteWhere [ PageScraped ==. False ]
                    deleteBy (UniquePage (optSeed opts))
            flip runSqlPool pool $ insertUnique $ Page "" (optSeed opts) False now
            forever $ do 
                maybePage <- flip runSqlPool pool $ selectFirst [ PageScraped ==. False ] [ Asc PageTimestamp ]
                case maybePage of 
                    Nothing -> exitSuccess
                    (Just (Entity pageId Page{..})) -> do
                        now <- getCurrentTime 
                        putStrLn pageTo
                        hrefs <- getPageDetails pageTo
                        flip runSqlPool pool $ update pageId [ PageScraped =. True ]
                        forM_ hrefs $ \href -> do 
                            flip runSqlPool pool $ insertUnique $ Page pageTo href False now