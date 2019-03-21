{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Tag where

import Import
import Yesod.Form.Bootstrap3

-- Tag form to create a new Tag
tagForm :: AForm Handler Tag
tagForm = Tag
    <$> areq textField (bfs ("Tag name" :: Text)) Nothing

getCreateTagR :: Handler Html
getCreateTagR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm tagForm
    defaultLayout $ do
            $(widgetFile "tags/create")

postCreateTagR :: Handler Html
postCreateTagR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm tagForm
    case res of
        FormSuccess tag -> do
            _ <- runDB $ insert tag
            redirect CreateTagR
        _ -> defaultLayout $ do
            $(widgetFile "tags/create")

-- Form to assign a tag to an article
assignTagForm :: AForm Handler TagArticle
assignTagForm = TagArticle
    <$> areq intField (bfs ("Article Id" :: Text)) Nothing
    <*> areq intField (bfs ("Tag Id" :: Text))     Nothing

getAssignTagR :: Handler Html
getAssignTagR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm assignTagForm
    defaultLayout $ do
        $(widgetFile "tags/assign")

postAssignTagR :: Handler Html
postAssignTagR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm assignTagForm
    case res of
        FormSuccess tagArticle -> do
            _ <- runDB $ insert tagArticle
            redirect AssignTagR
        _ -> defaultLayout $ do
            $(widgetFile "tags/assign")