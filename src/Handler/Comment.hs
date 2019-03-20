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
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        setTitle "Haskell Blog"
        $(widgetFile "Comment/comment")

postCommentR :: Handler Value
postCommentR = do

    comment <- (requireJsonBody :: Handler Comment)
    let uComment = comment
    returnJson uComment


commentIds :: (Text, Text, Text)
commentIds = ("commentForm", "commentTextarea", "commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]

