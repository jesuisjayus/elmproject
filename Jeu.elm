module Jeu exposing(..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html exposing (Html, Attribute, div, input, text)
import Html.Events exposing (..)
import Http
import Random 

import Json.Decode exposing (..)




-- MAIN


main =
  Browser.element{ init = init, update = update, subscriptions = subscriptions, view = view}



-- MODEL

type State = Failure | Loading | Success String

type alias Datas = { word : String, meanings : List Meaning}
type alias Meaning = {partOfSpeech : String, definitions : List Definition}
type alias Definition = {definition : String}

type alias Model = 
    { http : State
    , jSon : State
    , lWords : List String
    , word : String
    , datas : List Datas 
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Loading Loading [] "" []
  , Http.get
      { url = "http://localhost:8000/src/thousand_words.txt"
      , expect = Http.expectString GotWords
      }
  )



-- UPDATE


type Msg
  = GotWords (Result Http.Error String)
  | RandomWord Int
  | GotJson (Result Http.Error (List Datas))
  

  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotWords result ->
      case result of
        Ok words ->
          ({ model | lWords = String.words words , http = Success "" } , Random.generate RandomWord (Random.int 1 1000))

        Err _ ->
          ({model | http = Failure}, Cmd.none)
          
    RandomWord index -> case (getElementAtIndex model.lWords index) of
                                Nothing -> (model, Cmd.none)
                                Just x -> ({ model | word = x }, Http.get {url = ("https://api.dictionaryapi.dev/api/v2/entries/en/" ++ x)  , expect = Http.expectJson GotJson lDatasDecoder})
     
    GotJson result -> case result of
                            Ok data-> ({ model | jSon = Success "" , datas = data} , Cmd.none)

                            Err _ -> ({ model | jSon = Failure } , Cmd.none)   



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW
view : Model -> Html Msg
view model =
  div [style "text-align" "center"]
    [ h1 [] [ text "Guess it !" ]
    , viewWord model
    ]

viewWord : Model -> Html Msg
viewWord model =
  case model.http of
    Failure -> text "I was unable to load a word." 

    Loading -> text "Loading..."

    Success good -> overlay model (
      case model.jSon of
        Success veryGood -> textDatas model.datas
        Loading -> [text "Loading..."]
        Failure -> [text "I was unable to load a definition."] )


-- HELPERS

getElementAtIndex : List a -> Int -> Maybe a
getElementAtIndex list index =
    if index < 0 || index >= List.length list then
        Nothing
    else
        List.head (List.drop index list)
        
textDatas : (List Datas) -> List (Html Msg)
textDatas datas = 
  case datas of 
    [] -> []
    (x :: xs) -> [li [] ([text "Definitions"] ++ [ul [] (textMeaning x.meanings)])] ++ (textDatas xs)
    
textMeaning : List Meaning -> List (Html Msg)
textMeaning meanings = 
  case meanings of
    [] -> []
    (x :: xs) -> [li [] [text x.partOfSpeech]] ++ [ol [] (textDef x.definitions)] ++ (textMeaning xs)
    
textDef : List Definition -> List (Html Msg)
textDef def = 
  case def of
    [] -> []
    (x :: xs) -> [li [] [text x.definition]] ++ (textDef xs)   
    
overlay : Model -> List (Html Msg) -> Html Msg
overlay model txt = 
  div [] txt
          
-- JSON 

lDatasDecoder : Decoder (List Datas)
lDatasDecoder = Json.Decode.list datasDecoder

datasDecoder : Decoder Datas
datasDecoder = map2 Datas (field "word" string)(field "meanings" <| Json.Decode.list meaningDecoder)

meaningDecoder : Decoder Meaning
meaningDecoder = map2 Meaning (field "partOfSpeech" string)(field "definitions" <| Json.Decode.list definitionDecoder)

definitionDecoder : Decoder Definition
definitionDecoder = Json.Decode.map Definition (field "defintition" string)
          
