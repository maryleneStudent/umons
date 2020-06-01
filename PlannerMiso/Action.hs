module Action where

import           Miso
import           Network.URI

import           Model
import           Data.Map
import           Miso.String

data Action
    = FetchPlanners
    | Populate User
    | SetPlanners (Either String [Planner])
    | FetchAppointments User
    | SetAppointments (Either String (Map String [Appointment]))
    | SaveAppointment String String String String User
    | HandleURI URI
    | ChangeURI URI
    | SavePlanner Planner
    | SetAppointment Appointment
    | SetRegisterInfo (Either String (Map String RegisterInfo))
    | SetPlanner (Either String Planner)
    | ClickLogIn
    | ClickRegisterUser
    | DisplayRegistrationView
    | FetchUserInfo
    | UpdateLogin LoginInfo
    | SetLogin (Either String LoginInfo)
    | SetUser UserInfo
    | SetUsername MisoString
    | SetPassword MisoString
    | SetFirstName MisoString
    | SetLastName MisoString
    | LogOut
    | NoOp

initAction :: Action
initAction = NoOp --Populate User {uId = initialUserId, uPerson = initialPerson }
