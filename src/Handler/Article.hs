{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Article where

import Import
import Data.Text()
import Control.Applicative   ((<$>), (<*>))
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,)

-- Article form to create an article
articleForm :: Maybe Article -> AForm Handler Article
articleForm article = Article
    <$> areq intField  "Article id" Nothing
    <*> areq intField  "User id"    Nothing
    <*> areq textField "Content"    Nothing
    <*> areq textField "Title"      Nothing
    <*> aopt intField  "Score"      Nothing

getCreateArticleR :: Handler Html
getCreateArticleR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ articleForm Nothing
    defaultLayout
        [whamlet|
            <p>Create a new Article
            <form method=post action=@{CreateArticleR} enctype=#{enctype}>
                ^{widget}
                <button>Submit article
        |]

postCreateArticleR :: Handler Html
postCreateArticleR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ articleForm Nothing
    case res of
        FormSuccess article -> do
            _ <- runDB $ insert article
            redirect CreateArticleR
        _ -> defaultLayout
            [whamlet|
                <p>Error creating the article, try again
                <form method=post action=@{CreateArticleR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit article
            |]

-- Data to delete an article
data DeleteArticle = DeleteArticle
    { delArticleId :: Int
    , delUserId    :: Int
    } deriving (Show, Eq)

-- Getter articleId from delete form
getArticleId (DeleteArticle delArticleId _) = delArticleId

-- Getter userId from delete form
getUserId (DeleteArticle _ delUserId) = delUserId

deleteArticleForm :: Maybe DeleteArticle -> AForm Handler DeleteArticle
deleteArticleForm deleteArticle = DeleteArticle
    <$> areq intField "Article ID" Nothing
    <*> areq intField "User ID" Nothing

getDeleteArticleR :: Handler Html
getDeleteArticleR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ deleteArticleForm Nothing
    defaultLayout
        [whamlet|
            <p>Here you can delete an article
            <form method=post action=@{DeleteArticleR} enctype=#{enctype}>
                ^{widget}
                <button>Delete article
        |]

postDeleteArticleR :: Handler Html
postDeleteArticleR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ deleteArticleForm Nothing
    case res of
        FormSuccess article -> do
            _ <- runDB $ deleteWhere [ArticleArticleId ==. getArticleId article, ArticleUserId ==. getUserId article]
            redirect CreateArticleR
        _ -> defaultLayout
            [whamlet|
                <p>Error deleting the article, try again
                <form method=post action=@{DeleteArticleR} enctype=#{enctype}>
                    ^{widget}
                    <button>Delete article
            |]

-- Data to update an article
data UpdateArticle = UpdateArticle
    { updUserId    :: Int
    , updArticleId :: Int
    , title     :: Text
    , content   :: Text
    } deriving (Show, Eq)

-- Getter articleId from update form
getArticleIdUpdate (UpdateArticle _ updArticleId _ _) = updArticleId

-- Getter userId from update form
getUserIdUpdate (UpdateArticle updUserId _ _ _) = updUserId

-- Getter title from update form
getTitleUpdate (UpdateArticle _ _ title _) = title

-- Getter content from update form
getContentUpdate (UpdateArticle _ _ _ content) = content

updateArticleForm :: Maybe UpdateArticle -> AForm Handler UpdateArticle
updateArticleForm updateArticle = UpdateArticle
    <$> areq intField  "User id"    Nothing
    <*> areq intField  "Article id" Nothing
    <*> areq textField "Title"      Nothing
    <*> areq textField "Content"    Nothing

getUpdateArticleR :: Handler Html
getUpdateArticleR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ updateArticleForm Nothing
    defaultLayout
        [whamlet|
            <p>Here you can update an article
            <form method=post action=@{UpdateArticleR} enctype=#{enctype}>
                ^{widget}
                <button>Update article
        |]

postUpdateArticleR :: Handler Html
postUpdateArticleR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ updateArticleForm Nothing
    case res of
        FormSuccess article -> do
            _ <- runDB $ updateWhere [ArticleArticleId ==. getArticleIdUpdate article, ArticleUserId ==. getUserIdUpdate article] [ArticleTitle =. getTitleUpdate article, ArticleContent =. getContentUpdate article]
            redirect CreateArticleR
        _ -> defaultLayout
            [whamlet|
                <p>Error updating the article, try again
                <form method=post action=@{DeleteArticleR} enctype=#{enctype}>
                    ^{widget}
                    <button>Update article
            |]