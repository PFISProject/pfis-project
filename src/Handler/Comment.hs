{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Comment where

import Import
import Text.Julius 
import Text.Julius (RawJS (..))

getCommentR :: Handler Html
getCommentR = do
    allComments <- runDB $ getComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId, upVoteId, downVoteId) = commentIds
        let _ =  $(juliusFileReload "templates/Comment/comment.julius")
        setTitle "Haskell Blog"
        $(widgetFile "Comment/comment")

postCommentR :: Handler Value
postCommentR = do

    comment <- (requireJsonBody :: Handler Comment)
    commentId <- runDB $ insert comment
    let uComment = Entity commentId comment
    returnJson uComment


commentIds :: (Text, Text, Text, Text, Text)
commentIds = ("commentForm", "commentTextarea", "commentList", "upVote", "downVote")

getComments :: DB [Entity Comment]
getComments = selectList [] [Asc CommentId]

