{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Worlds
    ( main
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

main :: IO ()
main =
  do [dest] <- getArgs
     createDirectory dest
     medleys <- toListOf performances <$> get medleyRoot
     msrs <- toListOf performances <$> get msrRoot
     print medleys
     print msrs
     for_ (medleys <> msrs) $ \(Performance perfName path) ->
       do T.putStrLn perfName
          download (bbc <> path ^. from packed) "tmp.flv"
          extractAudio "tmp.flv" (dest </> perfName ^. from packed <> ".mp4")
  where medleyRoot = "http://www.bbc.co.uk/programmes/p02zrvzr"
        msrRoot = "http://www.bbc.co.uk/programmes/p02zr6z7"
        bbc = "http://www.bbc.co.uk"

performances :: Fold (Response ByteString) Performance
performances = responseBody . to (decodeUtf8With lenientDecode) . html . links . to table . _Just
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
