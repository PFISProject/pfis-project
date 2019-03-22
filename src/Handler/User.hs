{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import
import Yesod.Form.Bootstrap3

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
            tag <- runDB $ selectList [TagName ==. searchTagName search] []
            defaultLayout [whamlet|#{show tag}|]
        _ -> defaultLayout $ do
            $(widgetFile "articles/search")