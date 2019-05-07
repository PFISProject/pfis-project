{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Comment where

import Import
import Yesod.Form.Bootstrap3

data AssingCommentToArticle = AssingCommentToArticle
    { assignComment :: Text
    }

-- Form to comment an article
commentForm :: AForm Handler AssingCommentToArticle
commentForm = AssingCommentToArticle
    <$> areq textField (bfs ("Comment" :: Text)) Nothing

getAssignCommentR :: ArticleId -> Handler Html
getAssignCommentR articleId = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm commentForm
    defaultLayout $ do
        let actionR = AssignCommentR articleId
        $(widgetFile "comment/create")

postAssignCommentR :: ArticleId -> Handler Html
postAssignCommentR articleId = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm commentForm
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert (Comment (assignComment comment) articleId)
            redirect $ ShowArticleR articleId
        _ -> defaultLayout $ do
            let actionR = AssignCommentR articleId
            $(widgetFile "comment/create")
