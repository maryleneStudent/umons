module Planner exposing (Data, Entries, Planner, PlannerId, emptyEntries, emptyPlanner, entriesToList, getAvailableDays, getRemainingDays, idParser, idToString, listStringToEntries, plannerDecoder, plannerEncoder, plannersDecoder, setKey, stringToEntries, toDataList, toDataListWithKey)

import Credential exposing (Person, emptyPerson, personDecoder)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (dict, string)
import Url.Parser exposing (Parser, custom)


type alias Planner =
    { id : PlannerId
    , doctorId : String
    , person : Person
    , availableDays : Dict String Entries
    , remainingDays : Dict String Entries
    }


type alias Data =
    { key : String
    , value : String
    }


type Entries
    = Entries (List String)


type PlannerId
    = PlannerId String


idParser : Parser (String -> a) a
idParser =
    custom "PLANNERID" <|
        \plannerId ->
            Just plannerId


idDecoder : Decoder PlannerId
idDecoder =
    Decode.map PlannerId Decode.string


idToString : PlannerId -> String
idToString (PlannerId id) =
    id


entriesToList : Entries -> List String
entriesToList (Entries e) =
    e


stringToEntries : String -> Entries
stringToEntries s =
    Entries (List.singleton s)


listStringToEntries : List String -> Entries
listStringToEntries ls =
    Entries ls


getAvailableDays : Planner -> Dict String Entries
getAvailableDays planner =
    planner.availableDays


getRemainingDays : Planner -> Dict String Entries
getRemainingDays planner =
    planner.remainingDays


toDataList : List String -> List Data
toDataList data =
    List.map buildData data


toDataListWithKey : String -> List Data -> List Data
toDataListWithKey key data =
    List.map (setKey key) data


setKey : String -> Data -> Data
setKey id data =
    { key = id, value = data.value }


buildData : String -> Data
buildData data =
    { key = "", value = data }


plannersDecoder : Decoder (List Planner)
plannersDecoder =
    list plannerDecoder


entriesDecoder : Decoder Entries
entriesDecoder =
    Decode.succeed Entries
        |> required "entries" (list Decode.string)


plannerDecoder : Decoder Planner
plannerDecoder =
    Decode.succeed Planner
        |> required "id" idDecoder
        |> required "doctorId" Decode.string
        |> required "person" personDecoder
        |> required "availableDays" availableDaysDecoder
        |> required "remainingDays" availableDaysDecoder


availableDaysDecoder : Decoder (Dict String Entries)
availableDaysDecoder =
    Decode.dict
        entriesDecoder


plannerEncoder : Planner -> Encode.Value
plannerEncoder planner =
    Encode.object
        [ ( "id", encodeId planner.id )
        , ( "availableDays", stringDict planner.availableDays )
        , ( "remainingDays", stringDict planner.remainingDays )
        ]


stringDict : Dict String Entries -> Encode.Value
stringDict =
    dict identity encodeEntries


encodeEntries : Entries -> Encode.Value
encodeEntries (Entries entries) =
    Encode.object
        [ ( "entries", Encode.list Encode.string entries ) ]


encodeId : PlannerId -> Encode.Value
encodeId (PlannerId id) =
    Encode.string id


emptyEntries : Entries
emptyEntries =
    Entries []


emptyPlanner : Planner
emptyPlanner =
    { id = emptyPlannerId
    , doctorId = ""
    , person = emptyPerson
    , availableDays = Dict.empty
    , remainingDays = Dict.empty
    }


emptyPlannerId : PlannerId
emptyPlannerId =
    PlannerId ""
