{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Model where

import           Data.Aeson
import           Miso
import           Miso.String
import           Network.URI
import           Data.Map
import           GHC.Generics
import           Miso          hiding (defaultOptions)

data Model = Model
    { planners    :: Either String [Planner]
    , appointments :: Either String (Map String [Appointment])
    , appointment :: Appointment
    , auth :: AuthenticationStatus
    , mtoken :: Maybe String
    , mUsername :: MisoString
    , mPassword :: MisoString
    , mfirstname :: MisoString
    , mlastname :: MisoString
    , register :: Bool
    , isAdmin :: Bool
    , currentURI :: URI
    , registerErrorMsg :: MisoString
    , loginErrorMsg :: MisoString
    } deriving (Eq, Show)

data AuthenticationStatus
  = LoggedIn String User
  | LoggedOut
  | Unknown
  deriving (Show, Eq)

initialModel :: URI -> Model
initialModel uri = Model
    { planners = Left "Loading..."
    , appointments = Left "Loading..."
    , auth = LoggedOut
    , mtoken = Nothing
    , mUsername = pack ""
    , mPassword = pack ""
    , mfirstname = pack ""
    , mlastname = pack ""
    , register = False
    , appointment = emptyAppointment
    , isAdmin = False
    , currentURI = uri
    , registerErrorMsg = pack ""
    , loginErrorMsg = pack ""
    }

emptyAppointment :: Appointment
emptyAppointment = Appointment 
    { aId = ""
    , plannerId = ""
    , aDoctorId = ""
    , date = ""
    , hour = ""
    , userId = ""
    , aPerson = emptyPerson }

emptyPerson :: Person
emptyPerson = Person
    { firstname = ""
    , lastname = "" }

data User = User 
    { uId :: String
    , uPerson :: Person } deriving (Eq, Show)

type PlannerId = String

data Planner = Planner
    { id :: String
    , doctorId :: String
    , person :: Person
    , availableDays :: Map String Entries
    , remainingDays :: Map String Entries
    } deriving (Eq, Show, Generic)

type Entry = String

newtype Entries = Entries [String] deriving (Eq, Show, Generic)

instance FromJSON Entries where
  parseJSON (Object o) =
    Entries <$> (o .: "entries")


data Person = Person
    { firstname :: String
    , lastname :: String } deriving (Eq, Show, Generic)

data Appointment = Appointment
     { aId :: String 
     , plannerId :: String
     , aDoctorId :: String
     , date :: String
     , hour :: String
     , userId :: String
     , aPerson :: Person } deriving (Eq, Show, Generic)

data LoginInfo
  = LoginInfo { timestamp :: MisoString
    , status :: Int
    , error :: Maybe MisoString
    , message :: Maybe MisoString
    , id_token :: Maybe String
    , is_admin :: Maybe Bool}  deriving (Show, Eq, Generic)


data UserInfo
  = UserInfo {
      uiId :: String
    , uiPerson :: Person}  deriving (Show, Eq, Generic)

data RegisterInfo
    = RegisterInfo { regId :: MisoString
                   , regUsername :: MisoString
                   , regPassword :: MisoString
                   , regPerson :: Person } deriving (Show, Eq, Generic)

data Cred
    = Cred { cUsername :: MisoString
           , cPassword :: MisoString
           , cPerson :: Person } deriving (Show, Eq, Generic)

instance FromJSON Cred

instance FromJSON LoginInfo where
  parseJSON = genericParseJSON Data.Aeson.defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON UserInfo where
  parseJSON (Object o) =
   UserInfo <$>
    o .: "id"      <*>
    o .: "person"

instance FromJSON RegisterInfo where
  parseJSON (Object o) =
   RegisterInfo <$>
    o .: "id"       <*>
    o .: "username" <*>
    o .: "password" <*>
    o .: "person"

instance FromJSON Appointment where
  parseJSON (Object o) =
   Appointment <$>
    o .: "id"          <*>
    o .: "plannerId"   <*>
    o .: "doctorId"    <*>
    o .: "date"        <*>
    o .: "hour"        <*>
    o .: "userId"      <*>
    o .: "person"
 
instance FromJSON Planner where
  parseJSON (Object o) =  
   Planner <$>
    o .: "id"            <*>
    o .: "doctorId"      <*>
    o .: "person"        <*>
    o .: "availableDays" <*>
    o .: "remainingDays" 

instance FromJSON Person

instance ToJSON Planner

instance ToJSON Person

instance ToJSON Entries

instance ToJSON RegisterInfo

instance ToJSON Cred where
  toJSON (Cred cUsername cPassword cPerson) = object
        [ "username"    .= cUsername
        , "password"    .= cPassword
        , "person"      .= cPerson
        ]

instance ToJSON Appointment where
  toJSON (Appointment aId plannerId aDoctorId date hour userId aPerson) = object
        [ "id"          .= aId
        , "plannerId"   .= plannerId
        , "doctorId"    .= aDoctorId
        , "date"        .= date
        , "hour"        .= hour
        , "userId"      .= userId
        , "person"      .= aPerson
        ]
