{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Article where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql

-- Article form to create an article
articleForm :: AForm Handler Article
articleForm = Article
    <$> areq textField (bfs ("Title"   :: Text)) Nothing
    <*> areq textField (bfs ("Content" :: Text)) Nothing
    <*> areq intField  (bfs ("User id" :: Text)) Nothing
    <*> aopt intField  (bfs ("Score"   :: Text)) Nothing

getCreateArticleR :: Handler Html
getCreateArticleR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm articleForm
    defaultLayout $ do
        $(widgetFile "articles/create")

postCreateArticleR :: Handler Html
postCreateArticleR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm articleForm
    case res of
        FormSuccess article -> do
            articleId <- runDB $ insert article
            redirect $ ShowArticleR articleId
        _ -> defaultLayout $ do
            $(widgetFile "articles/create")

-- DELETE Handler
deleteArticleDeleteR :: ArticleId -> Handler Html
deleteArticleDeleteR articleId = do
    runDB $ delete articleId
    redirect CreateArticleR

updateArticleForm :: AForm Handler Article
updateArticleForm = Article
    <$> areq textField (bfs ("Title"   :: Text)) Nothing
    <*> areq textField (bfs ("Content" :: Text)) Nothing
    <*> areq intField  (bfs ("User id" :: Text)) Nothing
    <*> aopt intField  (bfs ("Score" :: Text)) Nothing

getUpdateArticleR :: ArticleId -> Handler Html
getUpdateArticleR articleId = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm updateArticleForm
    defaultLayout $ do
        let actionR = UpdateArticleR articleId
        $(widgetFile "articles/update")

postUpdateArticleR :: ArticleId -> Handler Html
postUpdateArticleR articleId = do
    article <- runDB $ get404 articleId 
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm updateArticleForm
    case res of
        FormSuccess articleResult -> do
            _ <- runDB $ replace articleId articleResult
            redirect $ ShowArticleR articleId
        _ -> defaultLayout $ do
            let actionR = UpdateArticleR articleId
            $(widgetFile "articles/update")

getShowArticleR :: ArticleId -> Handler Html
getShowArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        $(widgetFile "/articles/show")