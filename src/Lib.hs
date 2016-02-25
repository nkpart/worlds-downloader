{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Lens hiding (children)
import Data.ByteString.Lazy (ByteString)
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

someFunc :: IO ()
someFunc =
  do [dest] <- getArgs
     createDirectory dest
     response <- get root
     let perfs = recentPackages response

     for_ perfs $ \(Performance perfName path) ->
       do T.putStrLn perfName
          download (bbc <> path ^. from packed) "tmp.flv"
          extractAudio "tmp.flv" (dest </> perfName ^. from packed <> ".mp4")
  where root = "http://www.bbc.co.uk/programmes/p02zrvzr"
        bbc = "http://www.bbc.co.uk"


recentPackages :: Response ByteString -> [Performance]
recentPackages = toListOf $ responseBody . to (decodeUtf8With lenientDecode) . html . links . to table . _Just
  where links = allAttributed (ix "data-linktrack" . only "programmeobjectlink=cliptitle")

table :: Element -> Maybe Performance
table row =
   do let n = row ^. children . traverse . contents
      href <- row ^? attr "href" . _Just
      return $! Performance n href

download :: String -> FilePath -> IO ()
download uri fp = callProcess "youtube-dl" ["-o", fp, uri]

extractAudio :: FilePath -> FilePath -> IO ()
extractAudio flv mp4 = callProcess "ffmpeg" ["-i", flv, "-vn", mp4]
