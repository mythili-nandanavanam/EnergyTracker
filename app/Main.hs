{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, object, (.=), (.:), withObject)
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types (status200)

data EnergyData = EnergyData
  { device :: Text
  , consumption :: Int
  } deriving (Show)

instance ToJSON EnergyData where
  toJSON (EnergyData d c) =
    object ["device" .= d, "consumption" .= c]

instance FromJSON EnergyData where
  parseJSON = withObject "EnergyData" $ \v ->
    EnergyData <$> v .: "device" <*> v .: "consumption"

main :: IO ()
main = do
  ref <- newIORef ([] :: [EnergyData])

  let corsPolicy = corsWithContentType $ simpleCorsResourcePolicy
        { corsOrigins = Nothing  -- Allow any origin
        , corsRequestHeaders = ["Content-Type"]
        , corsMethods = ["GET", "POST", "OPTIONS"]
        , corsMaxAge = Just 3600  -- Cache preflight for 1 hour
        }
      
  putStrLn "Starting Energy Tracker backend server on port 3000..."
  putStrLn "Visit http://localhost:3000 to check if the server is running"

  scotty 3000 $ do
    middleware corsPolicy

    get "/" $ do
      html "<h1>Energy Tracker Backend is Running ⚡</h1><p>API endpoints available at /data and /clear</p>"

    get "/data" $ do
      entries <- liftIO $ readIORef ref
      json entries

    post "/data" $ do
      entry <- jsonData :: ActionM EnergyData
      liftIO $ modifyIORef ref (\xs -> xs ++ [entry])
      json entry

    post "/clear" $ do
      liftIO $ writeIORef ref []
      json ("All data cleared!" :: String)

-- Helper function to ensure Content-Type header is handled properly in CORS
corsWithContentType :: CorsResourcePolicy -> Middleware
corsWithContentType policy = cors (const $ Just policy)