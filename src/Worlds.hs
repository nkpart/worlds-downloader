{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
module Worlds
    ( main
    ) where

import Control.Lens             hiding (children, argument)
import Data.ByteString.Lazy     (ByteString)
import Control.Monad (unless)
import Text.Printf
import Data.Text as Text
import qualified Data.Map.Strict as M
import Path
import Path.IO
import Data.String
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding  (decodeUtf8With)
import Network.Wreq
import System.Process
import Text.Taggy.Lens
import Options.Applicative
import qualified Sound.HTagLib as TL

data Log =
    PageGet Text
  deriving (Eq, Show)

data Args =
      Slurp FilePath FilePath
    deriving (Eq, Show)

data Performance =
     Performance { performanceName :: Text, performanceUri :: Text }
     deriving (Eq, Show)

data Downloaded = Downloaded {
  downloadedPerformance :: Performance,
  downloadedPath :: Path Abs File
  }
  deriving (Eq, Show)

data Year = Y2019 | Y2018 | Y2017 | Y2016
  deriving (Eq, Show, Ord)

data Metadata = Metadata Year Text
  deriving (Eq, Show)

data Albums = 
       Albums (M.Map Year [(Downloaded, Metadata)])

instance Semigroup Albums where
  Albums m <> Albums n  = Albums $
    M.unionWith (<>) m n

instance Monoid Albums where mempty = Albums M.empty

cli :: Parser Args
cli = subparser
    (command "slurp" (info (slurpOptions <**> helper ) ( progDesc "Slurp from the BBC" )))
    where 
        slurpOptions = Slurp <$> (strOption (long "audio-place" <> help "the place to put the slurped audio"))
                             <*> (strOption (long "album-place" <> help "the place to put the albums"))

main :: IO ()
main = execParser cliPlus >>= \x -> case x of 
  Slurp d e -> slurp d e
 where cliPlus = info (cli <**> helper) (fullDesc)

slurp :: FilePath -> FilePath -> IO ()
slurp dest' albumPath =
  do 
     audioSaveDir <- parseRelDir dest'
     albumPathDir <- parseRelDir albumPath
     createDirIfMissing True audioSaveDir
     performances <- foldMap scrapeClips $ fmap clipsPage [1..5]
     xs <- traverse (downloadPerformance audioSaveDir) performances
     let Albums albums = foldMap addToAlbum xs
     itraverse_ (saveAlbum albumPathDir) albums
     pure ()

  where clipsPage :: Int -> String
        clipsPage n = printf "https://www.bbc.co.uk/programmes/b00dp4m5/clips?page=%d" n

addToAlbum :: Downloaded -> Albums
addToAlbum d = 
  let m@(Metadata year _title) = classifyClip d
   in Albums (M.singleton year [(d, m)])

tags :: Int -> Metadata -> TL.TagSetter
tags idx (Metadata year title) = 
  TL.titleSetter (TL.mkTitle title) <>
  TL.albumSetter (TL.mkAlbum $ albumTitle year) <>
  TL.yearSetter (TL.mkYear $ intYear year) <>
  TL.trackNumberSetter (TL.mkTrackNumber $ idx + 1)

albumTitle :: (Semigroup a, IsString a) => Year -> a
albumTitle y = "World Pipe Band Championships " <> displayYear y

displayYear :: IsString p => Year -> p
displayYear Y2016 = "2016"
displayYear Y2017 = "2017"
displayYear Y2018 = "2018"
displayYear Y2019 = "2019"

intYear :: Num a => Year -> a
intYear Y2016 = 2016
intYear Y2017 = 2017
intYear Y2018 = 2018
intYear Y2019 = 2019

saveAlbum :: Path x Dir -> Year -> [(Downloaded, Metadata)] -> IO ()
saveAlbum saveToDir year stuff = 
  let f idx (d, m) = do
         let target = targetDir </> filename (downloadedPath d)
             
         copyFile (downloadedPath d) target
         TL.setTags (toFilePath target) Nothing (tags idx m)

      Just albumDir = parseRelDir (show year)
      targetDir = saveToDir </> albumDir
  in do
     createDirIfMissing True targetDir
     itraverse_ f stuff

classifyClip :: Downloaded -> Metadata
classifyClip d = 
  let extractTitle = dropEnd (Text.length "-p044l3pf.m4a")
  in
  case fmap strip (splitOn "," (Text.pack (toFilePath (filename (downloadedPath d))))) of
    [_, "2016", trackName] -> Metadata Y2016 (extractTitle trackName)
    [_, "2017", trackName] -> Metadata Y2017 (extractTitle trackName)
    [_, "2018", trackName] -> Metadata Y2018 (extractTitle trackName)
    [_, trackName] -> Metadata Y2019 (extractTitle trackName)
    _ -> error (show d)

performanceTag :: Performance -> Text
performanceTag (Performance _ href) = 
  -- Clip urls look like https://www.bbc.co.uk/programmes/p06j1djf
  Text.reverse . Text.takeWhile (/= '/') . Text.reverse $ href

downloadPerformance :: Path x Dir -> Performance -> IO Downloaded
downloadPerformance audioSaveDir p =
        do workingDir <- (audioSaveDir </>) <$> parseRelDir (Text.unpack (performanceTag p))
           unlessDirExists workingDir $ do
            withSystemTempDir "worlds" $ \tmpDir -> do
              withCurrentDir tmpDir (downloadAudio (Text.unpack (performanceUri p)))
              createDirIfMissing True workingDir
              copyDirRecur tmpDir workingDir
           ([], [audioFile]) <- listDir workingDir
           pure $ Downloaded p audioFile

  where
  unlessDirExists :: () => Path b Dir -> IO () -> IO ()
  unlessDirExists d a = do
            ee <- doesDirExist d
            unless ee a

downloadAudio :: String -> IO ()
downloadAudio uri = callProcess "youtube-dl" ["-x", "--audio-quality", "0", uri]


----------------------
-- SCRAPER
----------------------

scrapeClips :: String -> IO [Performance]
scrapeClips page =
  do print page
     vs <- toListOf parsePerformances <$> get page
     let rejectTags = [
            "p06hm6q4" -- Trailer for 2018 Competition
          , "p05c3cz6" -- Trailer for 2017
          ]
     pure $! Prelude.filter (\p -> performanceTag p `notElem` rejectTags ) vs

parsePerformances :: Fold (Response ByteString) Performance
parsePerformances = 
  responseBody . 
  to (decodeUtf8With lenientDecode) . 
  html . clips . to parsePerformance . _Just
  where clips = allAttributed (ix "class" . only "programme__titles")

parsePerformance :: Element -> Maybe Performance
parsePerformance row =
   do let n = row ^. allNamed (only "a") . children . traverse . allNamed (only "span") . contents
      href <- row ^. allNamed (only "a") . attr "href"
      return $! (Performance n href)
