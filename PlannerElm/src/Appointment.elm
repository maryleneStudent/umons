module Appointment exposing (Appointment, appointmentDecoder, appointmentEncoder, appointmentsDecoder, emptyAppointment)

import Credential exposing (Person, emptyPerson, personDecoder, personEncoder)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (string)


type alias Appointment =
    { plannerId : String
    , doctorId : String
    , date : String
    , hour : String
    , userId : String
    , person : Person
    }


emptyAppointment : Appointment
emptyAppointment =
    { plannerId = ""
    , doctorId = ""
    , date = ""
    , hour = ""
    , userId = ""
    , person = emptyPerson
    }


appointmentsDecoder : Decoder (Dict String (List Appointment))
appointmentsDecoder =
    Decode.dict (Decode.list appointmentDecoder)


appointmentDecoder : Decoder Appointment
appointmentDecoder =
    Decode.succeed Appointment
        |> required "plannerId" Decode.string
        |> required "doctorId" Decode.string
        |> required "date" Decode.string
        |> required "hour" Decode.string
        |> required "userId" Decode.string
        |> required "person" personDecoder


appointmentEncoder : Appointment -> Encode.Value
appointmentEncoder appointment =
    Encode.object
        [ ( "plannerId", Encode.string appointment.plannerId )
        , ( "doctorId", Encode.string appointment.doctorId )
        , ( "date", Encode.string appointment.date )
        , ( "hour", Encode.string appointment.hour )
        , ( "userId", Encode.string appointment.userId )
        , ( "person", personEncoder appointment.person )
        ]
