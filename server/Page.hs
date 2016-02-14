{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page
  ( homePage
  , adminPage
  ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either (EitherT)

import Servant
import Servant.HTML.Blaze

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5.Attributes as A

import DB
import Types

renderPage' :: Maybe H.Html -> H.Html -> H.Html
renderPage' head body =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "x-ua-compatible" ! A.content "ie=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title "Fabian Rios"
      maybe mempty id head
    H.body body

renderPage :: H.Html -> H.Html
renderPage body = renderPage' Nothing body

renderBlogPost :: BlogPost -> H.Html
renderBlogPost BlogPost {..} =
  H.div $ do
    H.h2 $ H.toMarkup bpTitle
    H.p $ H.toMarkup bpBody

renderBlogPosts :: [BlogPost] -> H.Html
renderBlogPosts posts =
  H.div $ do
    foldMap renderBlogPost posts

homePage :: EitherT ServantErr IO H.Html
homePage = do
  blogPosts <- liftIO $ queryBlogPosts
  return . renderPage $ renderBlogPosts blogPosts

adminPage :: EitherT ServantErr IO H.Html
adminPage = return . renderPage $ do
  H.div ! A.id "admin" $ ""
  H.script ! A.src "/static/dist/main.js" $ ""
