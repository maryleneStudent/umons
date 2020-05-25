module User exposing (User)

import Credential exposing (Person)


type alias User =
    { id : String
    , person : Person
    }
