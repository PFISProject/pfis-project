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

getDeletePermsR :: UserId -> Handler Html
getDeletePermsR userId = do
    _ <- runDB $ update userId [UserPerms =. []]
    redirect ShowUsersR

getAssignUserPermsR :: UserId -> Handler Html
getAssignUserPermsR userId = do
    _ <- runDB $ update userId [UserPerms =. [PrvUser]]
    redirect ShowUsersR

getAssignAdminPermsR :: UserId -> Handler Html
getAssignAdminPermsR userId = do
    _ <- runDB $ update userId [UserPerms =. [PrvUser, PrvAdmin]]
    redirect ShowUsersR