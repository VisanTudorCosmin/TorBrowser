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
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.List (sort, isInfixOf)
import Data.Aeson.Encode.Pretty
import qualified Data.List as L
import Control.Arrow ((&&&))
import GHC.Exts
import Data.Function (on)
import Data.Aeson hiding (Options)
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import Data.Char

import System.Command
import System.Exit
import System.IO.Unsafe
import System.Random
import System.Random.Stateful
import Control.Exception
import System.Process (readProcessWithExitCode)

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
PageInfo 
    addr String
    title String
    keywords String
    deriving Generic
|]

data Node = Node {
    name :: String,
    group :: Int
} deriving (Generic, Show)

data Link = Link {
    source :: Int,
    target :: Int,
    value :: Int
} deriving (Generic, Show)

data JsonResponse = JsonResponse {
    nodes :: [Node],
    links :: [Link]
} deriving (Generic, Show)

instance ToJSON JsonResponse
instance ToJSON Node
instance ToJSON Link
instance ToJSON PageInfo

getBody :: String -> IO Stdout
getBody url = do 
    (exitcode, stdout, stderr) <- readProcessWithExitCode "curl" ["--socks5-hostname", "localhost:9150", url] ""
    if (exitcode == ExitSuccess) 
        then return $ Stdout stdout
        else return $ Stdout ""

getHrefs :: String -> [String]
getHrefs body = maybe [] id $ scrapeStringLike body (attrs "href" "a")

getAllWords :: String -> String
getAllWords body = maybe "" (L.intercalate ", ") $ scrapeStringLike body $ do 
    p <- texts "p"
    div <- texts "div"
    let total = L.sort $ take 20 $ filter (\el -> L.all isAlpha el) $ L.nub $ 
            filter (\el -> length el < 12 && length el > 5) $ 
            concat $ fmap words $ p ++ div
    return total 

getTitle :: String -> String
getTitle body = maybe "" id $ scrapeStringLike body $ text "title"

parseUriWithParent :: String -> String -> Maybe String
parseUriWithParent parent child = do 
    parentUri <- parseURIReference parent
    childUri <- parseURIReference child
    if uriIsRelative childUri 
        then return $ show $ relativeTo childUri parentUri 
        else return $ show childUri

getPageDetails :: String -> IO (String, String, [String])
getPageDetails url = do 
    Stdout out <- getBody url
    let keywords = getAllWords out
        links = filter isOnionUrl $ mapMaybe (parseUriWithParent url) (getHrefs out)
        title = getTitle out
    return $ (title, keywords, links)

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
countOccurences = map (head &&& length) . L.group . sort

getIndexWith :: [String] -> String -> Int
getIndexWith list el = maybe 0 id $ L.findIndex (el ==) list

rndInt :: IO Int
rndInt = randomRIO (1, 6)

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "data.db3" 10 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll

    runCommand $ \opts args -> do
        when (optExport opts) $ do
            putStrLn "Exporting data.."
            scrapedPages <- flip runSqlPool pool $ selectList [ PageScraped ==. True ] []
            pagesInfo <- flip runSqlPool pool $ selectList [] []
            let result = countOccurences $ mapMaybe pagesPairs scrapedPages
                pagesInfoContent = unlines $ fmap (\(Entity _ (PageInfo url title keywords)) -> url <> "," <> title <> "," <> keywords) pagesInfo
                exportContent = unlines $ fmap (\((from,to),count)-> from <> "," <> to <> "," <> (show count)) result
                jsonExport = fmap (\((from,to),count)-> (from, to)) result
                names = (L.nub (fmap fst jsonExport ++ fmap snd jsonExport))
                nodes = fmap (\n -> Node n (unsafePerformIO rndInt)) names 
                links = fmap (\(from,to) -> Link (getIndexWith names from) (getIndexWith names to) (unsafePerformIO rndInt)) jsonExport
            BL.writeFile "graph/result.json" $ encodePretty (JsonResponse nodes links)
            writeFile "data.csv" exportContent
            BL.writeFile "pageinfo.json" $ encodePretty (fmap entityVal pagesInfo)


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
                        flip runSqlPool pool $ update pageId [ PageScraped =. True ]
                        now <- getCurrentTime 
                        putStrLn pageTo
                        (title, keywords,hrefs) <- getPageDetails pageTo
                        flip runSqlPool pool $ insert $ PageInfo pageTo title keywords
                        forM_ hrefs $ \href -> do 
                            flip runSqlPool pool $ insertUnique $ Page pageTo href False now