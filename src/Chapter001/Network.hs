-- |

module Chapter001.Network where

-- web interface and json
import Network.Wreq
import Control.Lens

import Data.Aeson
import Data.Aeson.Lens

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as B

url :: String
url = "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"

query :: IO (Maybe B.ByteString)
query = do
    r <- getWith defaults url
    return $ r ^? responseBody
