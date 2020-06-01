{-# LANGUAGE RecordWildCards #-}
module Update where

import           Miso

import           Data.Function
import           Action
import           Effect
import           Model
import           Data.Map
import           Data.Either
import           Data.Maybe
import           Routing
import           Miso.String

updateModel :: Action -> Model -> Effect Action Model
updateModel action m@Model{..} = case action of
    FetchPlanners -> m <# do
      SetPlanners <$> fetchPlanners
    SetPlanners ePs -> noEff m { planners = ePs }
    FetchAppointments user -> m <# do SetAppointments <$> (fetchAppointments isAdmin $ uId user)
    Populate user -> step m user
    SetAppointments apps -> noEff m { appointments = apps }
    ChangeURI uri -> m <# do
      pushURI uri >> pure NoOp
    HandleURI uri -> noEff m { currentURI = uri }
    SaveAppointment k v pId docId u -> saveApp m k v pId docId u
    SetAppointment eA -> noEff m
    SetRegisterInfo eR -> case eR of
                             Left s -> noEff m {registerErrorMsg = pack "Erreur lors de l'enregistrement", mUsername = resetField, mPassword = resetField, mfirstname = resetField, mlastname = resetField}
                             Right j -> noEff m
    SetLogin loginInfo -> case loginInfo of
                            Left s -> noEff m {loginErrorMsg = pack "Identification impossible"}
                            Right l -> if Model.error l == Nothing then Update.log m l else noEff m {loginErrorMsg = pack "Identification impossible"}
    SetUser userInfo -> redirectToView m userInfo
    SetUsername usr -> noEff m { mUsername = usr}
    SetPassword pwd -> noEff m { mPassword = pwd}
    SetFirstName fn -> noEff m { mfirstname = fn }
    SetLastName ln -> noEff m { mlastname = ln }
    DisplayRegistrationView -> noEff m { register = True }
    ClickRegisterUser -> handleRegister m
    FetchUserInfo -> m <# do SetUser <$> getUserInfo (fromMaybe "" mtoken )
    ClickLogIn -> m <# do SetLogin <$> getLoginInfo cred
      where cred = Cred {cUsername = mUsername, cPassword = mPassword, cPerson = emptyPerson}
    LogOut -> batchEff newModel [pushURI topLink >> pure NoOp]
      where newModel = m {auth = LoggedOut, loginErrorMsg = resetField, registerErrorMsg = resetField, mUsername = resetField, mPassword = resetField, mfirstname = resetField, mlastname = resetField}
    NoOp -> noEff m

resetField :: MisoString
resetField = pack $ ""

updateAuth :: Maybe LoginInfo -> Model -> Model
updateAuth loginf m@Model{..} = m { auth = newAuth }
  where newAuth = if isJust loginf then Unknown else LoggedOut 

updateToken :: Maybe LoginInfo -> Model -> Model
updateToken loginf m@Model{..} = m { mtoken = newToken, isAdmin = updateIsAdmin }
  where newToken = case loginf of Just l -> id_token l
                                  Nothing -> Nothing
        updateIsAdmin = case loginf of Just l -> fromMaybe False (is_admin l)
                                       Nothing -> False

setLoggedIn :: Maybe UserInfo -> Model -> Model
setLoggedIn userInfo m@Model{..} = m { auth = newAuth }
  where newAuth = case userInfo of Just ui -> LoggedIn (fromMaybe "" mtoken) (User{ uId = (uiId ui), uPerson = (uiPerson ui)})
                                   Nothing -> LoggedOut

getUserFromStatus :: AuthenticationStatus -> Maybe User
getUserFromStatus status =  case status of
  LoggedIn _ u -> Just u
  LoggedOut -> Nothing
  Unknown -> Nothing

handleRegister :: Model -> Effect Action Model
handleRegister m@Model{..} = batchEff newModel [SetRegisterInfo <$> registerUser cred, pushURI topLink >> pure NoOp]
      where cred = Cred {cUsername = mUsername, cPassword = mPassword, cPerson = newPerson}
            newPerson = Person {firstname = unpack mfirstname, lastname = unpack mlastname}
            newModel = m {register = False, registerErrorMsg = resetField, mUsername = resetField, mPassword = resetField, mfirstname = resetField, mlastname = resetField}

redirectToView :: Model -> UserInfo -> Effect Action Model
redirectToView m userInf = case newUser of Just nu -> step newModel nu
                                           Nothing -> noEff newModel
  where newModel = m & (setLoggedIn $ Just userInf)
        newAuth = auth newModel
        newUser = getUserFromStatus newAuth

saveApp :: Model -> String -> String -> String -> String -> User -> Effect Action Model
saveApp m k v pId docId u = batchEff m [do SetAppointment <$> saveAppointment newApp, SetPlanners <$> fetchPlanners, SetAppointments <$> (fetchAppointments (isAdmin m) (uId u)), pushURI listLink >> pure NoOp]
  where newApp = Appointment {aId = "", date = k, hour = v, userId = uId u, plannerId = pId, aDoctorId = docId, aPerson = uPerson u}

step :: Model -> User -> Effect Action Model
step m user = batchEff m [SetPlanners <$> fetchPlanners, SetAppointments <$> (fetchAppointments (isAdmin m) (uId user))] 

log :: Model -> LoginInfo -> Effect Action Model
log m loginInfo = batchEff newModel [SetUser <$> getUserInfo newToken, pushURI listLink >> pure NoOp]
                    where newModel = m & (updateAuth $ Just loginInfo)
                                       & (updateToken $ Just loginInfo)
                          newToken = fromMaybe "" $ mtoken newModel

