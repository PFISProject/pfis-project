{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Profile where

import Import
import Database.Persist.Sql

getProfileR :: UserId -> Handler Html
getProfileR userId = do
    let uId = fromSqlKey userId
    user <- runDB $ get404 userId
    articles <- runDB $ selectList [ArticleAuthor ==. (fromIntegral uId)] []
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile/show")
