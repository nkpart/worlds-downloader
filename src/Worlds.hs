{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Worlds
    ( main
    ) where

import           Control.Lens             hiding (children)
import           Data.ByteString.Lazy     (ByteString)
import           Data.Foldable
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO             as T
import           Data.Text.Lazy.Encoding  (decodeUtf8With)
import           Data.Text.Lens
import           Debug.Trace
import           Network.Wreq
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process
import           Text.Taggy.Lens

data Performance =
     Performance Text -- ^ name
                 Text -- ^ programme uri
     deriving (Eq, Show)

main :: IO ()
main =
  do [dest] <- getArgs
     createDirectory dest
     -- _2014s <- toListOf performances <$> get _2014
     -- perfs <- foldMap performancesAt [_2016medleys, _2016msrs]
     perfs <- foldMap performancesAt [_2017medleys, _2017msrs, _2017grade2]
     -- medleys <- toListOf performances <$> get medleyRoot
     -- msrs <- toListOf performances <$> get msrRoot
     for_ perfs $
       \(Performance perfName path) ->
         -- for_ (medleys <> msrs) $ \(Performance perfName path) ->
         do T.putStrLn perfName
            download (bbc <> path ^. from packed) "tmp.flv"
            -- extractAudio "tmp.flv" (dest </> perfName ^. from packed <> ".mp4")
            renameFile "tmp.flv"
                       (dest </> perfName ^. from packed <> ".flv")
  where medleyRoot = "http://www.bbc.co.uk/programmes/p02zrvzr"
        msrRoot = "http://www.bbc.co.uk/programmes/p02zr6z7"
        _2014 = "http://www.bbc.co.uk/programmes/b04dw148/clips"
        _2016medleys = "http://www.bbc.co.uk/programmes/p044g8w4"
        _2016msrs = "http://www.bbc.co.uk/programmes/p044g9f5"

        _2017medleys = "http://www.bbc.co.uk/programmes/p05c6t5y"
        _2017msrs = "http://www.bbc.co.uk/programmes/p05c6q8r"
        _2017grade2 = "http://www.bbc.co.uk/programmes/p05c708j"
        bbc = "http://www.bbc.co.uk"

performancesAt page =
  do print page
     toListOf performances <$> get page

performances :: Fold (Response ByteString) Performance
performances = responseBody . to (decodeUtf8With lenientDecode) . html . links . to table . _Just
  -- where links = allAttributed (ix "data-linktrack" . only "programmeobjectlink=cliptitle")
  where links = allAttributed (ix "data-linktrack" . only "programmeobject_epblock")
  -- think this is for 2014 only?
  -- where links = allAttributed (ix "data-linktrack" . only "programmeobjectlink=title")

table :: Element -> Maybe Performance
table row =
  traceShow row $
   do let n = row ^. children . traverse . Text.Taggy.Lens.elements . contents
      href <- row ^? attr "href" . _Just
      return $! traceShow (n, href) $ Performance n href

download :: String -> FilePath -> IO ()
download uri fp = callProcess "youtube-dl" ["-o", fp, uri]

extractAudio :: FilePath -> FilePath -> IO ()
extractAudio flv mp4 = callProcess "ffmpeg" ["-i", flv, "-vn", mp4]
