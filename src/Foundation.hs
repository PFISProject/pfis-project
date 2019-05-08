{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import qualified Data.CaseInsensitive     as CI
import qualified Data.List                as L
import qualified Data.Text.Encoding       as TE
import           Yesod.Auth.OAuth2.Google
import           Yesod.Auth.OpenId        (IdentifierType (Claimed), authOpenId)
import           Yesod.Core.Types         (Logger)
import qualified Yesod.Core.Unsafe        as Unsafe
import           Yesod.Default.Util       (addStaticContentExternal)

clientId :: Text
clientId = "420817418225-g4jtoq5o4g0tubqsnbmjp94vct97egh7.apps.googleusercontent.com"

clientSecret :: Text
clientSecret = "PbOAC1g2aXlYvN3TnsGkp3CU"

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel          :: Text
    , menuItemRoute          :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where

    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing   -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        
        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Create article"
                    , menuItemRoute = CreateArticleR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Search articles by tag"
                    , menuItemRoute = SearchArticleByTagR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "View user profile" 
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized :: Route App -> Bool -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _           = return Authorized
    isAuthorized HomeR _               = return Authorized
    isAuthorized FaviconR _            = return Authorized
    isAuthorized RobotsR _             = return Authorized
    isAuthorized (StaticR _) _         = return Authorized
    isAuthorized (ShowArticleR _) _    = return Authorized
    isAuthorized SearchArticleByTagR _ = return Authorized
    
    isAuthorized ProfileR _            = isAuthenticated 
    isAuthorized CreateArticleR _      = authorizedForPrivileges [PrvUser]
    isAuthorized (UpdateArticleR _) _  = authorizedForPrivileges [PrvUser]
    isAuthorized (ArticleDeleteR _) _  = authorizedForPrivileges [PrvUser]
    isAuthorized (AssignCommentR _) _  = authorizedForPrivileges [PrvUser]
    isAuthorized (AssignTagR _) _      = authorizedForPrivileges [PrvUser]
    isAuthorized (ShowUserR _) _       = authorizedForPrivileges [PrvAdmin]
    isAuthorized ShowUsersR _          = authorizedForPrivileges [PrvAdmin]

    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where

    breadcrumb :: Route App -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR      = return ("Home", Nothing)
    breadcrumb (AuthR _)  = return ("Login", Just HomeR)
    breadcrumb ProfileR   = return ("Profile", Just HomeR)
    breadcrumb ShowUsersR = return ("Users", Just HomeR)
    breadcrumb  _         = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent    = credsIdent creds
                , userPassword = Nothing
                , userPerms    = [PrvUser]
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [oauth2GoogleScoped ["email", "profile"] clientId clientSecret] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "Por favor primero acceda para ver esta página"
        Just _  -> Authorized

instance YesodAuthPersist App

authorizedForPrivileges :: [Privileges] -> Handler AuthResult
authorizedForPrivileges perms = do
    mu <- maybeAuth
    return $ case mu of
     Nothing -> Unauthorized "Por favor primero acceda para ver esta página"
     Just u@(Entity userId user) ->
       if hasPrivileges u perms
            then Authorized
            else Unauthorized "No tiene los permisos suficientes"

hasPrivilege :: Entity User -> Privileges -> Bool
hasPrivilege u p = hasPrivileges u [p]

hasPrivileges :: Entity User -> [Privileges] -> Bool
hasPrivileges (Entity _ user) perms = null (perms L.\\ userPerms user)

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
