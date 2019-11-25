{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sheetwalker where

import Network.Google.Sheets hiding (driveScope)
import Control.Lens
import System.IO ( stdout )
import Network.Google
import Network.Google.Drive
import Data.Aeson
import Data.Maybe
import Data.Text (Text, isInfixOf)
import Control.Monad
import Data.Semigroup

runGoog a = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (driveScope ! spreadsheetsScope))
  runResourceT . runGoogle env $ a

walkSheets :: Text -> IO [(Maybe Text, Text)]
walkSheets aDir' = runGoog $ walker aDir'
  where
  -- walker :: Text -> Google s [(Maybe Text, Text)]
  walker aDir = do
    theFolders <- fmap (^. flFiles) <$> send $ filesList & flQ .~ Just ("'"<>aDir<>"' in parents and mimeType='application/vnd.google-apps.folder'")
    theFiles <- fmap (^. flFiles) <$> send $ filesList & flQ .~ Just ("'"<>aDir<>"' in parents and mimeType='application/vnd.google-apps.spreadsheet'")
    let immDirs = ((,) <$> (^. fName) <*> (fromJust . (^. fId))) <$> theFolders
        imms = filter (not . isInfixOf "SOLVED" . fromMaybe "" . fst) $ ((,) <$> (^. fName) <*> (fromJust . (^. fId))) <$> theFiles
    nexts <- sequence $ walker . snd <$> immDirs
    return $ imms <> join nexts


-- getSheetCells :: Text -> IO ([[Text]])
getSheetCells :: FromJSON a => Text -> IO [[a]]
getSheetCells aSheet = runGoog $ do
  rv <- send $ spreadsheetsValuesGet aSheet "A1:Z1000"
  case sequence . fmap (sequence . fmap fromJSON) $ rv ^. vrValues of
    Success a -> return a
    Data.Aeson.Error a -> error $ "Couldn't parse result: " <> show a
  


someFunc :: IO ()
someFunc = putStrLn "someFunc"
