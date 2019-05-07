{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3()

getShowUsersR :: Handler Html
getShowUsersR = do
    users <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "users/showAll")