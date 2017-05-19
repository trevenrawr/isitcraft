
import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex exposing (..)
import String
import Json.Decode as Json
import Json.Encode as JE
import Dom
import Task
--import Signal
--import ElmFire



main =
  Html.program
    { init = init ! []
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map SetAutoState Autocomplete.subscription


-- MODEL
type alias Brewery =
  { title: String
  , craft : Bool
  , owner : String
  , founded : String
  , location : String
  , website : String
  }

breweries : List Brewery
breweries =
  [ Brewery "Fu Man Brew" True "Fu Man Brew" "2015" "Boulder, CO" ""
  , Brewery "Deschutes Brewery" True "Deschutes Brewery" "1988" "Bend, OR" "https://www.deschutesbrewery.com"
  , Brewery "Anheuser-Busch" False "Anheuser-Busch InBev" "1852" "St. Louis, MO" "http://www.anheuser-busch.com"
  ]

defaultBrewery : Brewery
defaultBrewery =
  (Brewery "" True "" "" "" "")


type alias Model =
  { breweries : List Brewery
  , autoState : Autocomplete.State
  , howManyToShow : Int
  , query : String
  , selectedBrewery : Maybe Brewery
  , showMenu : Bool
  }


init : Model
init =
  { breweries = breweries
  , autoState = Autocomplete.empty
  , howManyToShow = 5
  , query = ""
  , selectedBrewery = Nothing
  , showMenu = False
  }



-- UPDATE


type Msg
    = SetQuery String
    | SetAutoState Autocomplete.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectBreweryKeyboard String
    | SelectBreweryMouse String
    | PreviewBrewery String
    | OnFocus
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetQuery newQuery ->
      let
        showMenu =
          not << List.isEmpty <| (acceptableBreweries newQuery model.breweries)
      in
        { model | query = newQuery, showMenu = showMenu, selectedBrewery = Nothing } ! []

    SetAutoState autoMsg ->
      let
        (newState, maybeMsg) =
          Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState (acceptableBreweries model.query model.breweries)
        
        newModel =
          { model | autoState = newState }
      in
        case maybeMsg of
          Nothing ->
            newModel ! []

          Just updateMsg ->
            update updateMsg newModel

    HandleEscape ->
      let
        validOptions =
          not <| List.isEmpty (acceptableBreweries model.query model.breweries)

        handleEscape =
          if validOptions then
            model
              |> removeSelection
              |> resetMenu
          else
            { model | query = "" }
              |> removeSelection
              |> resetMenu

        escapedModel =
          case model.selectedBrewery of
            Just brewery ->
              if model.query == brewery.title then
                model
                  |> resetInput
              else
                handleEscape

            Nothing ->
              handleEscape
      in
        escapedModel ! []

    Wrap toTop ->
      case model.selectedBrewery of
        Just brewery ->
          update Reset model

        Nothing ->
          if toTop then
            { model
              | autoState = Autocomplete.resetToLastItem updateConfig (acceptableBreweries model.query model.breweries) model.howManyToShow model.autoState
              , selectedBrewery = List.head <| List.reverse <| List.take model.howManyToShow <| (acceptableBreweries model.query model.breweries)
            }
              ! []
          else
            { model
              | autoState = Autocomplete.resetToFirstItem updateConfig (acceptableBreweries model.query model.breweries) model.howManyToShow model.autoState
              , selectedBrewery = List.head <| List.take model.howManyToShow (acceptableBreweries model.query model.breweries)
            }
              ! []

    Reset ->
      { model | autoState = Autocomplete.reset updateConfig model.autoState, selectedBrewery = Nothing } ! []

    SelectBreweryKeyboard id ->
      let
        newModel =
          setQuery model id
            |> resetMenu
      in
        newModel ! []

    SelectBreweryMouse id ->
      let
        newModel =
          setQuery model id
            |> resetMenu
      in
        ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "brewery-input") )

    PreviewBrewery id ->
      { model | selectedBrewery = Just <| getBreweryAtId model.breweries id } ! []

    OnFocus ->
      model ! []

    NoOp ->
      model ! []


resetInput model =
  { model | query = "" }
    |> removeSelection
    |> resetMenu

removeSelection model =
  { model | selectedBrewery = Nothing }

getBreweryAtId breweries id =
  List.filter (\brewery -> brewery.title == id) breweries
    |> List.head
    |> Maybe.withDefault defaultBrewery

setQuery model id =
  { model
    | query = .title <| getBreweryAtId model.breweries id
    , selectedBrewery = Just <| getBreweryAtId model.breweries id
  }

resetMenu model =
  { model
    | autoState = Autocomplete.empty
    , showMenu = False
  }
    


-- Setting up the data filtering for autocomplete
acceptableBreweries : String -> List Brewery -> List Brewery
acceptableBreweries query brewery =
  let
    lowerQuery =
      String.toLower query
  in
    List.filter (String.contains lowerQuery << String.toLower << .title) brewery


-- When the menu updates...
updateConfig : Autocomplete.UpdateConfig Msg Brewery
updateConfig =
  Autocomplete.updateConfig
    { toId = .title
    , onKeyDown = 
      \code maybeId ->
        if code == 38 || code == 40 then
          Maybe.map PreviewBrewery maybeId
        else if code == 13 then
          Maybe.map SelectBreweryKeyboard maybeId
        else
          Just <| Reset
    , onTooLow = Just <| Wrap False
    , onTooHigh = Just <| Wrap True
    , onMouseEnter = \id -> Just <| PreviewBrewery id
    , onMouseLeave = \_ -> Nothing
    , onMouseClick = \id -> Just <| SelectBreweryMouse id
    , separateSelections = False
    }

-- VIEW


view : Model -> Html Msg
view model =
  let
    options =
      { preventDefault = True, stopPropagation = False }

    dec =
      (Json.map
        (\code ->
          if code == 38 || code == 40 then
            Ok NoOp
          else if code == 27 then
            Ok HandleEscape
          else
            Err "We can't handle that key."
        )
        keyCode
      )
        |> Json.andThen
          fromResult

    fromResult : Result String a -> Json.Decoder a
    fromResult result =
      case result of
        Ok val ->
          Json.succeed val

        Err reason ->
          Json.fail reason

    menu =
      if model.showMenu then
        [ viewMenu model ]
      else
        []

    query =
      case model.selectedBrewery of
        Just brewery ->
          brewery.title

        Nothing ->
          model.query

    activeDescendant attributes =
      case model.selectedBrewery of
        Just brewery ->
          (attribute "aria-activedescendant"
            brewery.title
          )
            :: attributes

        Nothing ->
          attributes

  in
    div []
      [ div [ class "six columns" ]
        (List.append
          [ input
            (activeDescendant
              [ onInput SetQuery
              , onFocus OnFocus
              , onWithOptions "keydown" options dec
              , value query
              , attribute "type" "text"
              , placeholder "brewery search"
              , id "brewery-input"
              , class "autocomplete-input u-full-width"
              , autocomplete False
              , attribute "aria-owns" "list-of-breweries"
              , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
              , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
              , attribute "role" "combobox"
              , attribute "aria-autocomplete" "list"
              ]
            )
            []
          ]
          menu
        )
      , div [ class "six columns" ]
          [ breweryInfo model ]
      ]

breweryInfo : Model -> Html Msg
breweryInfo model =
  case model.selectedBrewery of
    Just brewery ->
      let
        color =
          if brewery.craft then
            "craft"
          else
            "not-craft"

        yesno =
          if brewery.craft then
            String.concat [ brewery.title, " is craft!" ]
          else
            String.concat [ brewery.title, " is not craft." ]
      in
        div [ id "brewery-info" ]
          [ h4 [ class color ] [ Html.text yesno ]
          , Html.p [] [ Html.text (String.concat [ "Owned by: ", brewery.owner ]) ]
          ]

    Nothing ->
      Html.text ""


viewMenu : Model -> Html Msg
viewMenu model =
  div [ class "autocomplete-menu u-full-width" ]
    [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableBreweries model.query model.breweries)) ]



-- Autocomplete view
viewConfig : Autocomplete.ViewConfig Brewery
viewConfig =
  let
    customizedLi keySelected mouseSelected brewery =
      { attributes =
        [ classList [ ("autocomplete-item", True), ("key-selected", keySelected || mouseSelected) ] 
        , id brewery.title
        ]
      , children = [ Html.text brewery.title ]
      }
  in
    Autocomplete.viewConfig
      { toId = .title
      , ul = [ class "autocomplete-list" ]
      , li = customizedLi
      }