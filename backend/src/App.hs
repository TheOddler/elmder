module App (main) where

import Network.Wai.Handler.Warp (run)
import Servant (Application, serve)
import Web (apiProxy, endpoints)

webServer :: Application
webServer =
  serve apiProxy endpoints

main :: IO ()
main = run 8080 webServer
