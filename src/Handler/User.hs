{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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
        FormSuccess tag -> do
            maybeTag <- runDB $ getBy (UniqueTag (searchTagName tag))
            case maybeTag of
                Nothing -> defaultLayout [whamlet|<h1>There's no articles with #{searchTagName tag} tag|]
                _ -> do
                    let justTag = fromJust maybeTag
                    tagArticles <- runDB $ selectList [TagArticleTagId ==. entityKey justTag] []
                    let articleIds = map tagArticleArticleId (map entityVal tagArticles) -- Get articleId from every TagArticle entity in tagArticles
                    articles <- runDB $ selectList [ArticleId <-. articleIds] []
                    defaultLayout $ do
                        $(widgetFile "articles/showByTag")
        _ -> defaultLayout $ do
            $(widgetFile "articles/search")

-- Function to get the value from a Maybe
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Error"

getShowUserR :: UserId -> Handler Html
getShowUserR userId = do
    _ <- runDB $ get404 userId
    defaultLayout $ do
        $(widgetFile "users/show")
