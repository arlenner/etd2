module Main exposing (..)

import Browser
import Browser.Events
import Json.Decode
import Html exposing (Html, button, text, div, h1, img, li, ul, input, i)
import Html.Attributes exposing ( class, src, style, type_, placeholder, value, checked)
import Html.Events exposing (onInput, onClick)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon

---- MODEL ----

type alias Todo =
    { id : String
    , done : Bool
    , title : String
    , description : String        
    }

type alias Model =
    { todos : List Todo
    , addingTodo : Bool
    , todoToAdd : Todo
    }

emptyTodo : Todo
emptyTodo =
    { id = "" 
    , done = False
    , title = ""
    , description = ""
    }

init : ( Model, Cmd Msg )
init =
    ( { todos = 
    [
        { id = "0 - learn"
        , title = "Learn Elm"
        , description = "its my new favorite thing!"
        , done = True
        }
    ,   { id = "1 - start"
        , title = "Start My Front-End Dev Career"
        , description = "no giving up allowed."
        , done = False                  
        }
    ], addingTodo = False, todoToAdd = emptyTodo }, Cmd.none )



---- UPDATE ----


type Msg
    = AddTodo String String
    | CheckTodo String
    | ClearTodo String
    | ChangeTitle String
    | ChangeDesc String
    | StartTodoForm
    | CancelTodoForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo t d ->
            ({ model | todos = 
                { id = (String.fromInt (List.length model.todos)) ++ String.slice 0 5 t
                , done = False
                , title = model.todoToAdd.title
                , description = model.todoToAdd.description 
                } :: model.todos
                , todoToAdd = emptyTodo
                , addingTodo = False }, Cmd.none)

        CheckTodo i ->
            ({ model | todos = List.map 
                (\td -> 
                    if td.id == i
                    then 
                    { id = td.id
                    , done = not td.done
                    , title = td.title
                    , description = td.description
                    }
                    else td) model.todos }, Cmd.none)

        ClearTodo i ->
            ({ model | todos = List.filter (\td -> td.id /= i) model.todos }, Cmd.none)

        StartTodoForm ->
            ({ model | addingTodo = True }, Cmd.none)
        
        CancelTodoForm ->
            ({ model | addingTodo = False }, Cmd.none)
        
        ChangeTitle t ->
            ({ model | todoToAdd = 
            { id = model.todoToAdd.id
            , title = t
            , description = model.todoToAdd.description
            , done = False
            } }, Cmd.none)
        ChangeDesc d ->
            ({ model | todoToAdd = 
            { id = model.todoToAdd.id
            , title = model.todoToAdd.title
            , description = d
            , done = False
            } }, Cmd.none)

---- VIEW ----

vTodo : Todo -> Html Msg
vTodo td =
    li [ class (if td.done then "done list-item" else "list-item"), onClick (CheckTodo td.id)]
        [ Icon.viewStyled [ Icon.fa2x, style "color" "green" ] (if td.done then Icon.checkCircle else Icon.circle)            
        , div [ class "li-header" ] [ text td.title]
        , div [] [ text " - "]
        , div [ class "li-preview" ] [ text td.description ]
        , div [ onClick (ClearTodo td.id)] [ Icon.viewStyled [ Icon.fa2x, style "color" "red" ] Icon.ban]
        ]

vInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
vInput t ph c v msg = 
    input [ type_ t, placeholder ph, class c, value v, onInput msg ] []

vTodoForm : Todo -> Html Msg
vTodoForm todo =
    div [ class "form" ]
    [ h1 [] [ text "Add Todo" ] 
    , vInput "text" "title" "form-input" todo.title (ChangeTitle)
    , vInput "text" "description" "form-input" todo.description (ChangeDesc)
    , div [] 
        [ button [ onClick CancelTodoForm, class "form-btn" ] [ Icon.viewStyled [ Icon.lg, style "color" "red" ] Icon.ban ]
        , button [ onClick (AddTodo todo.title todo.description), class "form-btn"] [ Icon.viewStyled [ Icon.lg ] Icon.checkCircle ]
        ]
    ]

getTodos : List Todo -> Html Msg
getTodos todos =
    ul [ class "list", style "color" "black" ] (List.map vTodo todos)

view : Model -> Html Msg
view model =
    div []
        [ Icon.css
        , h1 [] [ text "My To-Do List" ] 
        , (if model.addingTodo then vTodoForm model.todoToAdd else div [ style "hidden" "true" ] [])
        , getTodos model.todos
        , button 
            [ onClick (if model.addingTodo then CancelTodoForm else StartTodoForm)
            , style "border" "none", style "background-color" "transparent" 
            ] [ Icon.viewStyled [ Icon.fa4x ] Icon.plusCircle]
        ]



---- PROGRAM ----

            
main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
