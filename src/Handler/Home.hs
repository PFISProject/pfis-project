{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    articles <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "homepage")

