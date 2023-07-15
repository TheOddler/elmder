module App (main) where

import Network.Wai.Handler.Warp (run)
import Servant (Application, serve)
import Web (apiProxy, routes)

webServer :: Application
webServer =
  serve apiProxy routes

main :: IO ()
main = run 8081 webServer
