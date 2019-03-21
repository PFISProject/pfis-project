{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Comment where

import Import
import Text.Julius (RawJS (..))

getCommentR :: Handler Html
getCommentR = do
    allComments <- runDB $ getComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        setTitle "Haskell Blog"
        $(widgetFile "Comment/comment")

postCommentR :: Handler Value
postCommentR = do

    comment <- (requireJsonBody :: Handler Comment)
    uComment <- runDB $ insertEntity comment
    returnJson uComment


commentIds :: (Text, Text, Text)
commentIds = ("commentForm", "commentTextarea", "commentList")

getComments :: DB [Entity Comment]
getComments = selectList [] [Asc CommentId]

