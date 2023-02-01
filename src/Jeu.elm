module Jeu exposing(..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Events exposing (..)
import Http
import Random 

import Json.Decode exposing (..)




-- MAIN


main =
  Browser.element{ init = init, update = update, subscriptions = subscriptions, view = view}



-- MODEL

type State = Failure String | Loading | Success String

type alias Datas = { word : String, meanings : List Meaning}
type alias Meaning = {partOfSpeech : String, definitions : List Definition}
type alias Definition = {definition : String}

type alias Model = 
    { http : State
    , jSon : State
    , lWords : List String
    , word : String
    , datas : List Datas
    , content : String
    , title : String
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Loading Loading [] "" [] "" "Guess It !"
  , Http.get
      { url = "http://localhost:8000/thousand_words.txt"
      , expect = Http.expectString GotWords
      }
  )



-- UPDATE


type Msg
  = GotWords (Result Http.Error String)
  | RandomWord Int
  | GotJson (Result Http.Error (List Datas))
  | Change String
  | ViewWord String

  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotWords result ->
      case result of
        Ok words ->
          ({ model | lWords = String.words words , http = Success "" } , Random.generate RandomWord (Random.int 1 1000))

        Err error ->
          ({model | http = Failure (toString error)}, Cmd.none)
          
    RandomWord index -> case (getElemList model.lWords index) of
                                Nothing -> (model, Cmd.none)
                                Just x -> ({ model | word = x }, Http.get {url = ("https://api.dictionaryapi.dev/api/v2/entries/en/" ++ x)  , expect = Http.expectJson GotJson lDatasDecoder})
     
    GotJson result -> case result of
                            Ok data-> ({ model | jSon = Success "" , datas = data} , Cmd.none)

                            Err error -> ({ model | jSon = Failure (toString error) } , Cmd.none)   

    Change newContent -> ({model | content = newContent}, Cmd.none)

    ViewWord word -> if model.title == model.word then
                       ({model | title = "Guess It !"}, Cmd.none)
                     else
                       ({model | title = word}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW
view : Model -> Html Msg
view model =
  div [style "text-align" "center"]
    [ h1 [style "font-size" "300%", style "color" "blue", style "font-family" "verdana"] [ text model.title ]
    , viewWord model
    ]

viewWord : Model -> Html Msg
viewWord model =
  case model.http of
    Failure error -> text ("I was unable to load a word." ++ error) 

    Loading -> text "Loading..."

    Success good -> overlay model (
      case model.jSon of
        Success veryGood -> textDatas model.datas
        Loading -> [text "Loading..."]
        Failure error -> [text ("I was unable to load the definition " ++ error)] )


-- HELPERS

toString : Http.Error -> String 
toString erreur = 
  case erreur of 
    Http.BadUrl err -> "BadUrl" ++ err
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "NetworkError"
    Http.BadStatus err -> "BadStatus" ++ String.fromInt err
    Http.BadBody err -> "BadBody" ++ err


getElemList : List a -> Int -> Maybe a
getElemList list index =
    if index < 0 || index >= List.length list then
        Nothing
    else
        List.head (List.drop index list)
        
textDatas : (List Datas) -> List (Html Msg)
textDatas datas = 
  case datas of 
    [] -> []
    (x :: xs) -> [li [style "margin-left" "50px"] ([text "Definition"] ++ [ul [] (textMeaning x.meanings)])] ++ (textDatas xs)
    
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
  div [] 
      [
       div [style "text-align" "left"]
         txt
       , div [style "text-align" "center"]
         [ div []
            [input [placeholder "Try to guess the word", Html.Attributes.value model.content, onInput Change] [] 
            ,
            if String.toLower model.content == String.toLower model.word then
              div []
              [
               div[style "color" "green"] [text "You guessed it !"]
               , 
               img [ src "http://localhost:8000/congrats.gif"] []
              ]
            else
               div [] [text ("You typed " ++ model.content) ]
            ,
             button [ onClick (ViewWord model.word) ] [ text "Show word" ]
            ]
         ]
      ]
    
  

          
-- JSON 

lDatasDecoder : Decoder (List Datas)
lDatasDecoder = Json.Decode.list datasDecoder

datasDecoder : Decoder Datas
datasDecoder = map2 Datas (field "word" string)(field "meanings" <| Json.Decode.list meaningDecoder)

meaningDecoder : Decoder Meaning
meaningDecoder = map2 Meaning (field "partOfSpeech" string)(field "definitions" <| Json.Decode.list definitionDecoder)

definitionDecoder : Decoder Definition
definitionDecoder = Json.Decode.map Definition (field "definition" string)
                  
