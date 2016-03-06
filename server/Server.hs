module Server where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)

import Servant

import Types (PostAPI, UserAPI, AppAPI)
import Page (homePage, adminPage)
import API (api, getBlogPosts, createBlogPost, createUser, login)

postServer :: Server PostAPI
postServer = getBlogPosts :<|> createBlogPost

userServer :: Server UserAPI
userServer = createUser :<|> login

server :: Server AppAPI
server = homePage
    :<|> adminPage
    :<|> postServer
    :<|> userServer
    :<|> serveDirectory "client"

app :: Application
app =  serve api server

runBlog :: IO ()
runBlog = run 8080 $ logStdout app
