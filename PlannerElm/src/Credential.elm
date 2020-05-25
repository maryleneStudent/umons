module Credential exposing (Credential, Person, credentialDecoder, emptyCredential, emptyPerson, personDecoder, personEncoder, userEncoder)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Credential =
    { id : String
    , username : String
    , password : String
    , token : String
    , errorMsg : String
    , person : Person
    }


type alias Person =
    { firstname : String
    , lastname : String
    }


credentialDecoder : Decoder Credential
credentialDecoder =
    Decode.succeed Credential
        |> required "id" Decode.string
        |> required "username" Decode.string
        |> required "password" Decode.string
        |> optional "token" Decode.string ""
        |> optional "errorMsg" Decode.string ""
        |> required "person" personDecoder


emptyPerson : Person
emptyPerson =
    { lastname = "", firstname = "" }


emptyCredential : Credential
emptyCredential =
    { id = "", username = "", password = "", token = "", errorMsg = "", person = emptyPerson }


userEncoder : Credential -> Encode.Value
userEncoder model =
    Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        , ( "person", personEncoder model.person )
        ]


personEncoder : Person -> Encode.Value
personEncoder model =
    Encode.object
        [ ( "firstname", Encode.string model.firstname )
        , ( "lastname", Encode.string model.lastname )
        ]


personDecoder : Decoder Person
personDecoder =
    Decode.succeed Person
        |> required "lastname" Decode.string
        |> required "firstname" Decode.string
