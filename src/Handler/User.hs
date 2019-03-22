{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.User where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql

-- Data to search articles by tag
data SearchArticleByTag = SearchArticleByTag
    {searchTagName :: Text
    } deriving (Eq)

searchArticleByTagForm :: AForm Handler SearchArticleByTag
searchArticleByTagForm = SearchArticleByTag
    <$> areq textField (bfs ("Tag name" :: Text)) Nothing

getSearchArticleByTagR :: Handler Html
getSearchArticleByTagR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm searchArticleByTagForm
    defaultLayout $ do
        $(widgetFile "articles/search")

postSearchArticleByTagR :: Handler Html
postSearchArticleByTagR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm searchArticleByTagForm
    case res of
        FormSuccess search -> do
            (tag:_) <- runDB $ selectList [TagName ==. searchTagName search] []
            articleIds <- runDB $ selectList [TagArticleTagId ==. fromIntegral (fromSqlKey (entityKey tag))] []
            defaultLayout $ do
                $(widgetFile "articles/showByTag")
        _ -> defaultLayout $ do
            $(widgetFile "articles/search")