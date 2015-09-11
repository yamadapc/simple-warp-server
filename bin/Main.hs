{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    putStrLn "Server running at http://127.0.0.1:9000"
    run 9000 app

app :: Application
app _req respond = respond $ responseLBS status200 headers body
 where
   headers = [ ("Content-Type", "text/plan")]
   body = "Hello World"
