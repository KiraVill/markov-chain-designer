module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Url

import Svg
import Svg.Attributes
import Svg.Events

import Array exposing (Array)

-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


-- MODEL


--type alias Model =
--  { key : Nav.Key
--  , url : Url.Url
--  }


--init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
--init flags url key =
--  ( Model key url, Cmd.none )

type alias Model = 
    { states : Array State
    , edges : List Edge
    , mode : WindowMode
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = 
    ( Model Array.empty [] (Normal Nothing), Cmd.none)


-- UPDATE


--type Msg
--  = LinkClicked Browser.UrlRequest
--  | UrlChanged Url.Url


--update : Msg -> Model -> ( Model, Cmd Msg )
--update msg model =
--  case msg of
--    LinkClicked urlRequest ->
--      case urlRequest of
--        Browser.Internal url ->
--          ( model, Nav.pushUrl model.key (Url.toString url) --)
--
--        Browser.External href ->
--          ( model, Nav.load href )
--
--    UrlChanged url ->
--      ( { model | url = url }
--      , Cmd.none
--      )


type Msg
    = AddState
    | AddEdge
    | SelectState Int
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest

    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        AddState -> 
            ({model | states = Array.push (State (Array.length model.states) "New State") model.states}
            , Cmd.none 
            )
            
        AddEdge ->
            case model.mode of 
                Normal (Just selected) -> 
                    ( {model | mode = AddTransition selected }
                    , Cmd.none 
                    )
                    
                _ -> ( model, Cmd.none )
            
        SelectState s ->
            case model.mode of
                Normal selected ->
                    ({model | mode = Normal (if (Just s == selected) then Nothing else Just s)}
                    , Cmd.none 
                    )
                    
                AddTransition from ->
                    --case (from /= s, makeEdge model.states from s) of
                    --    ( True, Just edge) -> 
                    --        ({model | mode = Normal Nothing, 
                    --                  edges = model.edges ++ [edge]}
                    --        , Cmd.none 
                    --        )      
                    --    ( False, _ ) -> ( {model | mode = Normal (Just s)}, Cmd.none )  
                    --    ( _, _ ) -> ( model, Cmd.none )
                    
                    case from /= s of 
                        True -> ({model | mode = Normal Nothing, 
                                          edges = model.edges ++ [Edge from s 0]}
                                , Cmd.none 
                                )
                        False -> ( {model | mode = Normal (Just s)}, Cmd.none )  
            
        UrlChanged _ -> ( model, Cmd.none )
        LinkClicked _ -> ( model, Cmd.none )
    



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


--view : Model -> Browser.Document Msg
--view model =
--  { title = "URL Interceptor"
--  , body =
--      [ text "The current URL is: "
--      , b [] [ text (Url.toString model.url) ]
--      , ul []
--          [ viewLink "/home"
--          , viewLink "/profile"
--          , viewLink "/reviews/the-century-of-the-self"
--          , viewLink "/reviews/public-opinion"
--          , viewLink "/reviews/shah-of-shahs"
--          ]
--      ]
--  }
  

--viewLink : String -> Html msg
--viewLink path =
--  li [] [ a [ href path ] [ text path ] ]
  
  
view : Model -> Browser.Document Msg
view model =
  { title = "Markov Map"
  , body =
    [ button [ onClick AddState ] [ text "Add state" ]
    --, div [] [ text (debugPrintStates model.states) ]
    , button [ onClick AddEdge ] [ text "Add transition" ]
    , div [] [ text (debugPrintEdges model.edges) ]
    , div [] [ text ("Selected Node: " ++ debugSelected model.states model.mode) ]
    , div [] [ Svg.svg [ Svg.Attributes.width "1000"
                       , Svg.Attributes.height "1000"
                       , Svg.Attributes.viewBox "0 0 1000 1000" ] 
                       (drawAll model)
             ]
    ]
  }




    
-- Graph details

type alias State =
    { position : Int
    , label : String
    }


type alias Edge = 
    { from : Int   -- representing the id/position of a State
    , to: Int
    , prob : Float
    }


type WindowMode 
    = Normal ( Maybe Int )
    | AddTransition Int


drawAll : Model -> List (Svg.Svg Msg)
drawAll m = 
    (List.foldl (drawState m.mode) [] (Array.toList m.states)) ++
    (List.foldl (drawEdge m.mode) [] m.edges)
    
 
drawState : WindowMode -> State -> List (Svg.Svg Msg) -> List (Svg.Svg Msg)
drawState mode state svgs =
    let 
        --x = String.fromInt (60 + 150 * (modBy 5 state.position))
        --y = String.fromInt (60 + 150 * (state.position // 5))
        (x,y) = getNodeCoords state.position
        --r = "40"
        clickResponse = SelectState state.position
    in
    svgs ++
    (case mode of
        AddTransition selected ->
            [ Svg.circle [ Svg.Attributes.cx (String.fromInt x)
                          , Svg.Attributes.cy (String.fromInt y)
                          , Svg.Attributes.r (String.fromInt styleNodeRadius)
                          , Svg.Attributes.fill "grey"
                          , Svg.Attributes.stroke "red"
                          , Svg.Attributes.strokeWidth (if selected == state.position then "6" else "2")
                          , Svg.Events.onClick clickResponse
                          ] [] ]      
    
        Normal (Just selected) -> 
            [ Svg.circle [ Svg.Attributes.cx (String.fromInt x)
                          , Svg.Attributes.cy (String.fromInt y)
                          , Svg.Attributes.r (String.fromInt styleNodeRadius)
                          , Svg.Attributes.fill "yellow"
                          , Svg.Attributes.stroke "red"
                          , Svg.Attributes.strokeWidth (if selected == state.position then "6" else "2")
                          , Svg.Events.onClick clickResponse
                          ] [] ]
        
        _ -> 
            [ Svg.circle [ Svg.Attributes.cx (String.fromInt x)
                          , Svg.Attributes.cy (String.fromInt y)
                          , Svg.Attributes.r (String.fromInt styleNodeRadius)
                          , Svg.Attributes.fill "yellow"
                          , Svg.Attributes.stroke "red"
                          , Svg.Attributes.strokeWidth "2"
                          , Svg.Events.onClick clickResponse
                          ] [] ]
        
    ) ++ [ Svg.text_ [ Svg.Attributes.x (String.fromInt x)
                     , Svg.Attributes.y (String.fromInt y)
                     --, Svg.Attributes.fill "blue"
                     , Svg.Attributes.textAnchor "middle"
                     , Svg.Attributes.dominantBaseline "middle"
                     , Svg.Events.onClick clickResponse
                     ] [Svg.text (trimLabel 10 state.label)] ] 
            

drawEdge : WindowMode -> Edge -> List (Svg.Svg Msg) -> List (Svg.Svg Msg)
drawEdge mode e svgs = 
    let 
        (x1,y1) = getNodeOutCoords e.from
        (x2,y2) = getNodeInCoords e.to
    in
    case mode of 
        Normal (Just selected) ->
            (Svg.line [ Svg.Attributes.x1 (String.fromInt x1)
                      , Svg.Attributes.y1 (String.fromInt y1)
                      , Svg.Attributes.x2 (String.fromInt x2)
                      , Svg.Attributes.y2 (String.fromInt y2)
                      , Svg.Attributes.stroke "red"
                      , Svg.Attributes.strokeWidth (if selected == e.from || selected == e.to then "6" else "2")
                      ] []) :: svgs
    
        _ ->
            (Svg.line [ Svg.Attributes.x1 (String.fromInt x1)
                      , Svg.Attributes.y1 (String.fromInt y1)
                      , Svg.Attributes.x2 (String.fromInt x2)
                      , Svg.Attributes.y2 (String.fromInt y2)
                      , Svg.Attributes.stroke "red"
                      ] []) :: svgs
                   


trimLabel : Int -> String -> String
trimLabel toLength label =
    if (String.length label <= toLength) then
        label
    else
        (String.left (toLength - 3) label) ++ "..."


styleNodeRadius = 40
styleNodeSpacing = 150
styleNodesPerRow = 5


getNodeCoords : Int -> (Int, Int)
getNodeCoords node = 
    (60 + styleNodeSpacing * (modBy styleNodesPerRow node)
    ,60 + styleNodeSpacing * (node // styleNodesPerRow)
    )


getNodeOutCoords : Int -> (Int, Int)
getNodeOutCoords node =
    let
        (x,y) = getNodeCoords node
    in 
    (x + styleNodeRadius, y)


getNodeInCoords : Int -> (Int, Int)
getNodeInCoords node =
    let
        (x,y) = getNodeCoords node
    in 
    (x - styleNodeRadius, y)





--makeEdge : Array State -> Int -> Int -> Maybe Edge
--makeEdge states index1 index2 = 
--    case (Array.get index1 states, Array.get index2 states) of
--        ( Just s1, Just s2) -> Just (Edge s1 s2 0)
--        ( _, _ ) -> Nothing


debugPrintStates : Array State -> String
debugPrintStates states =
    List.map (\n -> n.label) (Array.toList states)
        |> List.intersperse " "
        |> List.foldl (++) ""


debugPrintEdges : List Edge -> String
debugPrintEdges edges = 
    --List.map (\e -> "(" ++ e.from.label ++ " " ++ e.to.label ++ " " ++ (String.fromFloat e.prob) ++ ") ") edges
    List.map (\e -> "(" ++ (String.fromInt e.from) ++ "->" ++ (String.fromInt e.to) ++ " " ++ (String.fromFloat e.prob) ++ ") ") edges
        |> List.foldl (++) ""
   
   
debugSelected : Array State -> WindowMode -> String
debugSelected states mode =
    case mode of
        Normal (Just id) ->
            let maybeS = Array.get id states in
            case maybeS of 
                Nothing -> "Nothing"
                Just s -> s.label ++ (String.fromInt s.position)
        _ -> "Nothing"