{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Worlds
    ( main
    ) where

import Control.Lens hiding (children)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.Wreq
import Text.Taggy.Lens
import Data.Text
import qualified Data.Text.IO as T
import Data.Text.Lens
import Data.Monoid
import Data.Foldable
import System.Process
import System.Environment
import System.FilePath
import System.Directory

data Performance =
     Performance Text -- ^ name
                 Text -- ^ programme uri
     deriving (Eq, Show)

main :: IO ()
main =
  do [dest] <- getArgs
     createDirectory dest
     -- _2014s <- toListOf performances <$> get _2014
     perfs <- performancesAt _2016medleys <> performancesAt _2016msrs
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
        bbc = "http://www.bbc.co.uk"

performancesAt page =
     toListOf performances <$> get page

performances :: Fold (Response ByteString) Performance
performances = responseBody . to (decodeUtf8With lenientDecode) . html . links . to table . _Just
  where links = allAttributed (ix "data-linktrack" . only "programmeobjectlink=cliptitle")
  -- think this is for 2014 only?
  -- where links = allAttributed (ix "data-linktrack" . only "programmeobjectlink=title")

table :: Element -> Maybe Performance
table row =
  traceShow row $
   do let n = row ^. children . traverse . contents
      href <- row ^? attr "href" . _Just
      return $! traceShow n $ Performance n href

download :: String -> FilePath -> IO ()
download uri fp = callProcess "youtube-dl" ["-o", fp, uri]

extractAudio :: FilePath -> FilePath -> IO ()
extractAudio flv mp4 = callProcess "ffmpeg" ["-i", flv, "-vn", mp4]
