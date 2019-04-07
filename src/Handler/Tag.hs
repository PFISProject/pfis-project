{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Tag where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist

data AssignTagToArticle = AssignTagToArticle
    { assignTagName :: Text
    }

-- Form to assign a tag to an article
assignTagForm :: AForm Handler AssignTagToArticle
assignTagForm = AssignTagToArticle
    <$> areq textField (bfs ("Tag name" :: Text)) Nothing

getAssignTagR :: ArticleId -> Handler Html
getAssignTagR articleId = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm assignTagForm
    defaultLayout $ do
        let actionR = AssignTagR articleId
        $(widgetFile "tags/assign")

postAssignTagR :: ArticleId -> Handler Html
postAssignTagR articleId = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm assignTagForm
    case res of
        FormSuccess tag -> do
            maybeTag <- runDB $ getBy (UniqueTag (assignTagName tag))
            case maybeTag of
                Nothing -> do
                    _ <- runDB $ do
                        tagId <- insert $ (Tag (assignTagName tag))
                        insert $ (TagArticle articleId tagId)
                    redirect $ ShowArticleR articleId
                _ -> do
                    let justTag = fromJust maybeTag
                    _ <- runDB $ do
                        insert $ (TagArticle articleId (entityKey justTag))
                    redirect $ ShowArticleR articleId
        _ -> defaultLayout $ do
            let actionR = AssignTagR articleId
            $(widgetFile "tags/assign")

-- Function to get the value from a Maybe
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Error"