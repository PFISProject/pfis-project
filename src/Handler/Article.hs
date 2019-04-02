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
    <$> areq textField (bfs ("Title" :: Text))      Nothing
    <*> areq textField (bfs ("Content" :: Text))    Nothing
    <*> areq intField  (bfs ("User id" :: Text))    Nothing
    <*> aopt intField  (bfs ("Score" :: Text))      Nothing

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

-- Data to delete an article
data DeleteArticle = DeleteArticle
    { delArticleId :: Int
    } deriving (Show, Eq)

deleteArticleForm ::AForm Handler DeleteArticle
deleteArticleForm = DeleteArticle
    <$> areq intField (bfs ("Article ID" :: Text)) Nothing

getDeleteArticleR :: Handler Html
getDeleteArticleR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm deleteArticleForm
    defaultLayout $ do
        $(widgetFile "articles/delete")

postDeleteArticleR :: Handler Html
postDeleteArticleR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm deleteArticleForm
    case res of
        FormSuccess article -> do
            _ <- runDB $ deleteWhere [ArticleId ==. toSqlKey (fromIntegral (delArticleId article))]
            redirect CreateArticleR
        _ -> defaultLayout $ do
            $(widgetFile "articles/delete")

-- Data to update an article
data UpdateArticle = UpdateArticle
    {updArticleId :: ArticleId
    , title     :: Text
    , content   :: Text
    } deriving (Show, Eq)

updateArticleForm :: AForm Handler UpdateArticle
updateArticleForm = UpdateArticle
    <$> areq intField  (bfs ("Article id" :: Text)) Nothing
    <*> areq textField (bfs ("Title" :: Text))      Nothing
    <*> areq textField (bfs ("Content" :: Text))    Nothing

getUpdateArticleR :: Handler Html
getUpdateArticleR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm updateArticleForm
    defaultLayout $ do
        $(widgetFile "articles/update")

postUpdateArticleR :: Handler Html
postUpdateArticleR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm updateArticleForm
    case res of
        FormSuccess article -> do
            _ <- runDB $ updateWhere [ArticleId ==. article updArtcileId] [ArticleTitle =. title article, ArticleContent =. content article]
            redirect CreateArticleR
        _ -> defaultLayout $ do
                $(widgetFile "articles/update")

getShowArticleR :: ArticleId -> Handler Html
getShowArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        $(widgetFile "/articles/show")