{-# LANGUAGE OverloadedStrings #-}
module Effect where

import qualified Codec.Binary.UTF8.String      as U
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.JSString                 as J
import           JavaScript.Web.XMLHttpRequest
import           Data.Map
import           Model

fetchPlanners :: IO (Either String [Planner])
fetchPlanners = do
    Just json <- contents <$> xhrByteString req
    pure $ eitherDecodeStrict json
    where
        req = Request
            { reqMethod = GET
            , reqURI = "http://localhost:3001/planners"
            , reqLogin = Nothing
            , reqHeaders = []
            , reqWithCredentials = False
            , reqData = NoData
            }

fetchAppointments :: Bool -> String -> IO (Either String (Map String [Appointment]))
fetchAppointments isAdmin id = do
    Just json <- contents <$> xhrByteString req
    pure $ eitherDecodeStrict json
    where
        req = Request
            { reqMethod = GET
            , reqURI = J.pack $ getBaseUrl isAdmin ++ id ++ "/appointments"
            , reqLogin = Nothing
            , reqHeaders = []
            , reqWithCredentials = False
            , reqData = NoData
            }

getBaseUrl :: Bool -> String
getBaseUrl isAdmin = if isAdmin == True then "http://localhost:3001/doctor/" else "http://localhost:3001/patient/"

saveAppointment :: Appointment -> IO Appointment
saveAppointment app = do
      Just resp <- contents <$> xhrByteString req
      case eitherDecodeStrict resp :: Either String Appointment of
        Left s -> Prelude.error s
        Right j -> pure j
    where
        req = Request
            { reqMethod = POST
            , reqURI = J.pack $ "http://localhost:3001/appointment/create"
            , reqLogin = Nothing
            , reqHeaders = [("Content-type", "application/json")]
            , reqWithCredentials = False
            , reqData = StringData . J.pack . U.decode . B.unpack . encode $ app
}

getLoginInfo :: Cred -> IO (Either String LoginInfo)
getLoginInfo cred = do
  Just resp <- contents <$> xhrByteString req
  pure $ eitherDecodeStrict resp
  where
    req = Request { reqMethod = POST
                  , reqURI = J.pack "http://localhost:3001/login"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = StringData . J.pack . U.decode . B.unpack . encode $ cred
                  }

registerUser :: Cred -> IO (Either String (Map String RegisterInfo))
registerUser cred = do
  Just json <- contents <$> xhrByteString req
  pure $ eitherDecodeStrict json
  where
    req = Request { reqMethod = POST
                  , reqURI = J.pack "http://localhost:3001/users/sign-up"
                  , reqLogin = Nothing
                  , reqHeaders = [("Content-type", "application/json")]
                  , reqWithCredentials = False
                  , reqData = StringData . J.pack . U.decode . B.unpack . encode $ cred
                  }

getUserInfo :: String -> IO UserInfo
getUserInfo token = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String UserInfo of
    Left s -> Prelude.error s
    Right j -> pure j
  where
    req = Request { reqMethod = GET
                  , reqURI = J.pack "http://localhost:3001/users/me"
                  , reqLogin = Nothing
                  , reqHeaders = [("Authorization", J.pack token)]
                  , reqWithCredentials = False
                  , reqData = NoData
                  }
