{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3()

getShowUsersR :: Handler Html
getShowUsersR = do
    users <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "users/showAll")
