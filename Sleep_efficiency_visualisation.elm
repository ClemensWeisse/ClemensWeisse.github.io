module Sleep_efficiency_visualisation exposing (main)

import Axis
import Browser
import Color
import Csv
import Date exposing (Interval(..))
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h1, h3, h4, option, pre, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Http
import List.Extra
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import Time exposing (Month(..))
import TypedSvg exposing (circle, g, line, style, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))



-- GLOBAL VARIABLES


xPlotPadding : Float
xPlotPadding =
    200


yPlotPadding : Float
yPlotPadding =
    100


w : Float
w =
    2150


h : Float
h =
    900


tickCount : Int
tickCount =
    10


gapBetweenVerticales : Float
gapBetweenVerticales =
    300


stickLimbColors : List Color.Color
stickLimbColors =
    [ Color.green, Color.orange, Color.purple, Color.red, Color.darkYellow ]


fontSizeParallelCoordInPx : Length
fontSizeParallelCoordInPx =
    Px 25


fontSizeCaptionsInPx : Length
fontSizeCaptionsInPx =
    Px 50


fontSizeHoverInPx : Length
fontSizeHoverInPx =
    Px 18



-- TYPE ALIASES


type alias Point =
    { pointName : String, x : Float, y : Float }


type alias PointData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List MultiDimPoint
    }



---- MODEL


type alias Model =
    { data : FileReceiving
    , selectedXAxisScatterPlot : String
    , selectedYAxisScatterPlot : String
    , selectedXAxisIconsPlot : String
    , selectedYAxisIconsPlot : String
    , selectedStick1IconsPlot : String
    , selectedStick2IconsPlot : String
    , selectedStick3IconsPlot : String
    , selectedStick4IconsPlot : String
    , selectedStick5IconsPlot : String
    , showScatterPlot : Bool
    , showParallelCoordinates : Bool
    , showIcons : Bool
    , orderOfDimensions : List Int
    }



-- TYPES


type FileReceiving
    = Failure
    | Loading
    | Success String


type Msg
    = GotText (Result Http.Error String)
    | XAxisScatterPlotSelected String
    | YAxisScatterPlotSelected String
    | XAxisIconsPlotSelected String
    | YAxisIconsPlotSelected String
    | Stick1IconsPlotSelected String
    | Stick2IconsPlotSelected String
    | Stick3IconsPlotSelected String
    | Stick4IconsPlotSelected String
    | Stick5IconsPlotSelected String
    | ScatterPlot
    | ParallelCoordinates
    | Icons
    | ChangeDimensions Int Int
    | ScatterPlotWithXY String String
    | IconsWithXY String String



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = Loading
      , selectedXAxisScatterPlot = "ID"
      , selectedYAxisScatterPlot = "Age"
      , selectedXAxisIconsPlot = "ID"
      , selectedYAxisIconsPlot = "Age"
      , selectedStick1IconsPlot = "Sleep duration"
      , selectedStick2IconsPlot = "Sleep efficiency"
      , selectedStick3IconsPlot = "REM sleep percentage"
      , selectedStick4IconsPlot = "Deep sleep percentage"
      , selectedStick5IconsPlot = "Light sleep percentage"
      , showScatterPlot = True
      , showParallelCoordinates = False
      , showIcons = False
      , orderOfDimensions = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
      }
    , getText
    )


getText : Cmd Msg
getText =
    Http.get
        { --   url = "http://localhost:8080/" ++ "Sleep_Efficiency.csv"
          url = "https://clemensweisse.github.io/" ++ "Sleep_Efficiency.csv"
        , expect = Http.expectString GotText
        }


csvString_to_data : String -> List (List String)
csvString_to_data csvRaw =
    Csv.split csvRaw



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | data = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | data = Failure }, Cmd.none )

        XAxisScatterPlotSelected value ->
            ( { model | selectedXAxisScatterPlot = value }, Cmd.none )

        YAxisScatterPlotSelected value ->
            ( { model | selectedYAxisScatterPlot = value }, Cmd.none )

        ScatterPlot ->
            ( { model | showScatterPlot = True, showParallelCoordinates = False, showIcons = False }, Cmd.none )

        ParallelCoordinates ->
            ( { model | showScatterPlot = False, showParallelCoordinates = True, showIcons = False }, Cmd.none )

        Icons ->
            ( { model | showScatterPlot = False, showParallelCoordinates = False, showIcons = True }, Cmd.none )

        ChangeDimensions dimOne dimeTwo ->
            ( { model | orderOfDimensions = List.Extra.swapAt dimOne dimeTwo model.orderOfDimensions }, Cmd.none )

        Stick1IconsPlotSelected value ->
            ( { model | selectedStick1IconsPlot = value }, Cmd.none )

        Stick2IconsPlotSelected value ->
            ( { model | selectedStick2IconsPlot = value }, Cmd.none )

        Stick3IconsPlotSelected value ->
            ( { model | selectedStick3IconsPlot = value }, Cmd.none )

        Stick4IconsPlotSelected value ->
            ( { model | selectedStick4IconsPlot = value }, Cmd.none )

        Stick5IconsPlotSelected value ->
            ( { model | selectedStick5IconsPlot = value }, Cmd.none )

        XAxisIconsPlotSelected value ->
            ( { model | selectedXAxisIconsPlot = value }, Cmd.none )

        YAxisIconsPlotSelected value ->
            ( { model | selectedYAxisIconsPlot = value }, Cmd.none )

        ScatterPlotWithXY xVal yVal ->
            ( { model | showScatterPlot = True, showParallelCoordinates = False, showIcons = False, selectedXAxisScatterPlot = xVal, selectedYAxisScatterPlot = yVal }, Cmd.none )

        IconsWithXY xVal yVal ->
            ( { model | showScatterPlot = False, showParallelCoordinates = False, showIcons = True, selectedXAxisIconsPlot = xVal, selectedYAxisIconsPlot = yVal }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ Html.Attributes.style "text-align" "center" ] [ text "Sleep Efficiency Visualisation" ]
        , h3 [ Html.Attributes.style "text-align" "center" ]
            [ text
                ("There are three visualisations of the dataset. "
                    ++ "How you can switch between them is always written as a hint above the plot. "
                    ++ "(Alternatively, you can of course switch the views using the buttons.)"
                )
            ]
        , br [] []
        , div []
            [ button [ Html.Events.onClick <| ScatterPlot, Html.Attributes.style "margin-left" "20px", Html.Attributes.style "margin-right" "10px" ] [ text "Show Scatterplot" ]
            , button [ Html.Events.onClick <| ParallelCoordinates, Html.Attributes.style "margin-left" "10px", Html.Attributes.style "margin-right" "10px" ] [ text "Show Parallel Coordinates" ]
            , button [ Html.Events.onClick <| Icons, Html.Attributes.style "margin-left" "10px", Html.Attributes.style "margin-right" "10px" ] [ text "Show Stick Figures" ]
            ]
        , br [] []
        , div
            [ if model.showScatterPlot then
                Html.Attributes.style "" ""
                -- Html.Attributes.style "display" "inline-block"

              else
                Html.Attributes.style "display" "none"

            -- Html.Attributes.style "visibility" "hidden"
            ]
            [ h4 [ Html.Attributes.style "text-align" "center" ]
                [ text
                    ("By clicking on a point, you will switch to the stick figures plot. "
                        ++ "By hovering over the point you can see how many datapoints are stacked on top of each other. "
                        ++ "In addition, points are then displayed larger and redder."
                    )
                ]
            , div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "margin-left" "50px", Html.Attributes.style "margin-right" "10px" ] [ text "Select X-Axis Value " ]
            , select [ onInput XAxisScatterPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedXAxisScatterPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedXAxisScatterPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedXAxisScatterPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedXAxisScatterPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedXAxisScatterPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedXAxisScatterPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedXAxisScatterPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedXAxisScatterPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedXAxisScatterPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedXAxisScatterPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedXAxisScatterPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedXAxisScatterPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedXAxisScatterPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedXAxisScatterPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedXAxisScatterPlot) ] [ text "Exercise frequency" ]
                ]
            , div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "margin-left" "50px", Html.Attributes.style "margin-right" "10px" ] [ text "Select Y-Axis Value " ]
            , select [ onInput YAxisScatterPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedYAxisScatterPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedYAxisScatterPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedYAxisScatterPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedYAxisScatterPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedYAxisScatterPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedYAxisScatterPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedYAxisScatterPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedYAxisScatterPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedYAxisScatterPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedYAxisScatterPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedYAxisScatterPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedYAxisScatterPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedYAxisScatterPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedYAxisScatterPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedYAxisScatterPlot) ] [ text "Exercise frequency" ]
                ]
            ]
        , br [] []
        , div
            [ if model.showParallelCoordinates then
                Html.Attributes.style "" ""

              else
                Html.Attributes.style "display" "none"
            ]
            [ h4 [ Html.Attributes.style "text-align" "center" ]
                [ text
                    ("By clicking on a caption above a vertical axis, you will switch to the scatter plot. "
                        ++ "The scatter plot will then have the same attribute you clicked as the x-axis and the attribute to the right of that attribute as the y-axis. "
                    )
                ]
            , h4 [ Html.Attributes.style "text-align" "center" ]
                [ text
                    ("Hovering over any element of the plot will show the labels of the axis. "
                        ++ "By clicking on one of the arrows you can switch the vertical axis with it's neighbors. "
                    )
                ]
            ]
        , br [] []
        , div
            [ if model.showIcons then
                Html.Attributes.style "" ""

              else
                Html.Attributes.style "display" "none"
            ]
            [ h4 [ Html.Attributes.style "text-align" "center" ]
                [ text
                    ("By clicking on a stick figure, you will switch to the parallel coordinates plot. "
                        ++ "With the help of the colors you can see which limb belongs to which attribute.  "
                    )
                ]
            , div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "margin-left" "50px", Html.Attributes.style "margin-right" "10px" ] [ text "Select X-Axis Value " ]
            , select [ onInput XAxisIconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedXAxisIconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedXAxisIconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedXAxisIconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedXAxisIconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedXAxisIconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedXAxisIconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedXAxisIconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedXAxisIconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedXAxisIconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedXAxisIconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedXAxisIconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedXAxisIconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedXAxisIconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedXAxisIconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedXAxisIconsPlot) ] [ text "Exercise frequency" ]
                ]
            , div [ Html.Attributes.style "display" "inline-block", Html.Attributes.style "margin-left" "50px", Html.Attributes.style "margin-right" "10px" ] [ text "Select Y-Axis Value " ]
            , select [ onInput YAxisIconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedYAxisIconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedYAxisIconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedYAxisIconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedYAxisIconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedYAxisIconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedYAxisIconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedYAxisIconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedYAxisIconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedYAxisIconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedYAxisIconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedYAxisIconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedYAxisIconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedYAxisIconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedYAxisIconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedYAxisIconsPlot) ] [ text "Exercise frequency" ]
                ]
            , br [] []
            , br [] []
            , div (htmlPropsDropDowns 0)
                [ Html.text "Select Limb 1 Value " ]
            , select [ onInput Stick1IconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedStick1IconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedStick1IconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedStick1IconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedStick1IconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedStick1IconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedStick1IconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedStick1IconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedStick1IconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedStick1IconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedStick1IconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedStick1IconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedStick1IconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedStick1IconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedStick1IconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedStick1IconsPlot) ] [ text "Exercise frequency" ]
                ]
            , div (htmlPropsDropDowns 1) [ Html.text "Select Limb 2 Value " ]
            , select [ onInput Stick2IconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedStick2IconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedStick2IconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedStick2IconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedStick2IconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedStick2IconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedStick2IconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedStick2IconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedStick2IconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedStick2IconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedStick2IconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedStick2IconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedStick2IconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedStick2IconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedStick2IconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedStick2IconsPlot) ] [ text "Exercise frequency" ]
                ]
            , div (htmlPropsDropDowns 2) [ Html.text "Select Limb 3 Value " ]
            , select [ onInput Stick3IconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedStick3IconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedStick3IconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedStick3IconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedStick3IconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedStick3IconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedStick3IconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedStick3IconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedStick3IconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedStick3IconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedStick3IconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedStick3IconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedStick3IconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedStick3IconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedStick3IconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedStick3IconsPlot) ] [ text "Exercise frequency" ]
                ]
            , div (htmlPropsDropDowns 3) [ Html.text "Select Limb 4 Value " ]
            , select [ onInput Stick4IconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedStick4IconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedStick4IconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedStick4IconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedStick4IconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedStick4IconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedStick4IconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedStick4IconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedStick4IconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedStick4IconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedStick4IconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedStick4IconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedStick4IconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedStick4IconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedStick4IconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedStick4IconsPlot) ] [ text "Exercise frequency" ]
                ]
            , div (htmlPropsDropDowns 4) [ Html.text "Select Limb 5 Value " ]
            , select [ onInput Stick5IconsPlotSelected ]
                [ option [ value "ID", selected ("ID" == model.selectedStick5IconsPlot) ] [ text "ID" ]
                , option [ value "Age", selected ("Age" == model.selectedStick5IconsPlot) ] [ text "Age" ]
                , option [ value "Gender", selected ("Gender" == model.selectedStick5IconsPlot) ] [ text "Gender" ]
                , option [ value "Bedtime", selected ("Bedtime" == model.selectedStick5IconsPlot) ] [ text "Bedtime" ]
                , option [ value "Wakeup time", selected ("Wakeup time" == model.selectedStick5IconsPlot) ] [ text "Wakeup time" ]
                , option [ value "Sleep duration", selected ("Sleep duration" == model.selectedStick5IconsPlot) ] [ text "Sleep duration" ]
                , option [ value "Sleep efficiency", selected ("Sleep efficiency" == model.selectedStick5IconsPlot) ] [ text "Sleep efficiency" ]
                , option [ value "REM sleep percentage", selected ("REM sleep percentage" == model.selectedStick5IconsPlot) ] [ text "REM sleep percentage" ]
                , option [ value "Deep sleep percentage", selected ("Deep sleep percentage" == model.selectedStick5IconsPlot) ] [ text "Deep sleep percentage" ]
                , option [ value "Light sleep percentage", selected ("Light sleep percentage" == model.selectedStick5IconsPlot) ] [ text "Light sleep percentage" ]
                , option [ value "Awakenings", selected ("Awakenings" == model.selectedStick5IconsPlot) ] [ text "Awakenings" ]
                , option [ value "Caffeine consumption", selected ("Caffeine consumption" == model.selectedStick5IconsPlot) ] [ text "Caffeine consumption" ]
                , option [ value "Alcohol consumption", selected ("Alcohol consumption" == model.selectedStick5IconsPlot) ] [ text "Alcohol consumption" ]
                , option [ value "Smoking status", selected ("Smoking status" == model.selectedStick5IconsPlot) ] [ text "Smoking status" ]
                , option [ value "Exercise frequency", selected ("Exercise frequency" == model.selectedStick5IconsPlot) ] [ text "Exercise frequency" ]
                ]
            ]
        , br [] []
        , div [] [ dataHandling model ]
        ]


htmlPropsDropDowns : Int -> List (Html.Attribute msg)
htmlPropsDropDowns num =
    [ Html.Attributes.style "display" "inline-block"
    , Html.Attributes.style "margin-left" "50px"
    , Html.Attributes.style "margin-right" "10px"
    , Html.Attributes.style "color" (Color.toCssString (stickLimbColors |> List.Extra.getAt num |> Maybe.withDefault Color.black))
    ]


dataHandling : Model -> Html Msg
dataHandling model =
    case model.data of
        Failure ->
            text "I was unable to load the Sleep_Efficiency dataset."

        Loading ->
            text "Loading..."

        Success fullText ->
            let
                linesOfDataSet =
                    csvString_to_data fullText

                filteredEmptyValuesDataSet =
                    List.filter
                        (List.all
                            (\str ->
                                if str /= "" then
                                    True

                                else
                                    False
                            )
                        )
                        linesOfDataSet

                headListData =
                    List.head filteredEmptyValuesDataSet |> Maybe.withDefault []

                tailListData =
                    List.tail filteredEmptyValuesDataSet |> Maybe.withDefault []

                headTailTranspData =
                    List.Extra.transpose filteredEmptyValuesDataSet

                filtTranspData =
                    tailListData
                        |> List.Extra.transpose
                        |> List.Extra.setAt 2 maleTo0FemaleTo1List
                        |> List.Extra.setAt 3 convertBedTimeToList
                        |> List.Extra.setAt 4 convertWakeupTimeToList
                        |> List.Extra.setAt 13 smokingNo0Yes1List

                maleTo0FemaleTo1List =
                    tailListData
                        |> List.Extra.transpose
                        |> List.Extra.getAt 2
                        |> Maybe.withDefault []
                        |> List.map
                            (\el ->
                                if el == "Male" then
                                    "0"

                                else
                                    "1"
                            )

                smokingNo0Yes1List =
                    tailListData
                        |> List.Extra.transpose
                        |> List.Extra.getAt 13
                        |> Maybe.withDefault []
                        |> List.map
                            (\el ->
                                if el == "No" then
                                    "0"

                                else
                                    "1"
                            )

                convertBedTimeToList =
                    convertTimeToFloatStringList 3

                convertWakeupTimeToList =
                    convertTimeToFloatStringList 4

                convertTimeToFloatStringList num =
                    tailListData
                        |> List.Extra.transpose
                        |> List.Extra.getAt num
                        |> Maybe.withDefault []
                        |> List.map (\el -> List.Extra.getAt 1 (String.split " " el) |> Maybe.withDefault "")
                        -- "2021-07-03 01:30:00" -> "01:30:00"
                        |> List.map
                            (\el ->
                                String.split ":" el
                                    -- "01:30:00" -> ["01", "30", "00"]
                                    |> List.take 2
                             -- ["01", "30", "00"] -> ["01", "30"]
                            )
                        |> List.map
                            (\li ->
                                (li
                                    |> List.Extra.getAt 0
                                    -- ["01", "30"] -> "01"
                                    |> Maybe.withDefault ""
                                    |> String.toFloat
                                    -- "01" -> 1
                                    |> Maybe.withDefault 0.0
                                    |> String.fromFloat
                                 -- 1 -> "1"
                                )
                                    ++ "."
                                    ++ (if (li |> List.Extra.getAt 1 |> Maybe.withDefault "") == "30" then
                                            -- ["01", "30"] -> "30"
                                            "5"
                                            -- "30" -> "5"

                                        else
                                            "0"
                                       )
                             -- ["01", "30"] -> "1.5"
                            )

                floatTuple : Int -> ( String, List Float )
                floatTuple num =
                    ( List.Extra.getAt num headTailTranspData |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""
                    , List.Extra.getAt num filtTranspData |> Maybe.withDefault [] |> List.map (String.toFloat >> Maybe.withDefault 0.0)
                    )

                ( xAxisHeadSP, xAxisTailSPList ) =
                    dropdownCases model.selectedXAxisScatterPlot

                ( yAxisHeadSP, yAxisTailSPList ) =
                    dropdownCases model.selectedYAxisScatterPlot

                ( xAxisHeadIP, xAxisTailIPList ) =
                    dropdownCases model.selectedXAxisIconsPlot

                ( yAxisHeadIP, yAxisTailIPList ) =
                    dropdownCases model.selectedYAxisIconsPlot

                ( stick1HeadIP, stick1TailIPList ) =
                    dropdownCases model.selectedStick1IconsPlot

                ( stick2HeadIP, stick2TailIPList ) =
                    dropdownCases model.selectedStick2IconsPlot

                ( stick3HeadIP, stick3TailIPList ) =
                    dropdownCases model.selectedStick3IconsPlot

                ( stick4HeadIP, stick4TailIPList ) =
                    dropdownCases model.selectedStick4IconsPlot

                ( stick5HeadIP, stick5TailIPList ) =
                    dropdownCases model.selectedStick5IconsPlot

                dropdownCases : String -> ( String, List Float )
                dropdownCases stringSubject =
                    case stringSubject of
                        "ID" ->
                            floatTuple 0

                        "Age" ->
                            floatTuple 1

                        "Gender" ->
                            floatTuple 2

                        "Bedtime" ->
                            floatTuple 3

                        "Wakeup time" ->
                            floatTuple 4

                        "Sleep duration" ->
                            floatTuple 5

                        "Sleep efficiency" ->
                            floatTuple 6

                        "REM sleep percentage" ->
                            floatTuple 7

                        "Deep sleep percentage" ->
                            floatTuple 8

                        "Light sleep percentage" ->
                            floatTuple 9

                        "Awakenings" ->
                            floatTuple 10

                        "Caffeine consumption" ->
                            floatTuple 11

                        "Alcohol consumption" ->
                            floatTuple 12

                        "Smoking status" ->
                            floatTuple 13

                        "Exercise frequency" ->
                            floatTuple 14

                        _ ->
                            ( "", [] )

                pointDataTuple =
                    listsToPointData
                        xAxisTailSPList
                        yAxisTailSPList
                        xAxisHeadSP
                        yAxisHeadSP

                movedIndexHeadList =
                    List.map (\num -> List.Extra.getAt num headListData |> Maybe.withDefault "") model.orderOfDimensions

                movedIndexTailList =
                    List.map
                        (\num ->
                            List.Extra.getAt num filtTranspData
                                |> Maybe.withDefault []
                        )
                        model.orderOfDimensions
                        |> List.Extra.transpose

                multiDimData =
                    listsToMultiDimData (List.map (\li -> List.map (String.toFloat >> Maybe.withDefault 0.0) li) movedIndexTailList) movedIndexHeadList

                iconsMultiDimData =
                    iconListsToMultiDimData
                        [ xAxisTailIPList
                        , yAxisTailIPList
                        , stick1TailIPList
                        , stick2TailIPList
                        , stick3TailIPList
                        , stick4TailIPList
                        , stick5TailIPList
                        ]
                        [ xAxisHeadIP
                        , yAxisHeadIP
                        , stick1HeadIP
                        , stick2HeadIP
                        , stick3HeadIP
                        , stick4HeadIP
                        , stick5HeadIP
                        ]
            in
            pre []
                [ div []
                    [ if model.showScatterPlot then
                        scatterplot (Tuple.first pointDataTuple) (Tuple.second pointDataTuple)

                      else
                        text ""
                    ]
                , div []
                    [ if model.showParallelCoordinates then
                        parallelCoodinatesPlot multiDimData

                      else
                        text ""
                    ]
                , div []
                    [ if model.showIcons then
                        iconsPlot iconsMultiDimData

                      else
                        text ""
                    ]
                ]


adjustLabels : String -> String
adjustLabels description =
    case description of
        "Age" ->
            description ++ " (in years)"

        "Gender" ->
            description ++ " (0: men, 1: women)"

        "Bedtime" ->
            description ++ " (in hours)"

        "Wakeup time" ->
            description ++ " (in hours)"

        "Sleep duration" ->
            description ++ " (in hours)"

        "Caffeine consumption" ->
            description ++ " (in mg)"

        "Alcohol consumption" ->
            description ++ " (in oz)"

        "Smoking status" ->
            description ++ " (0: nonsmokers, 1: smokers)"

        _ ->
            description


scatterplot : PointData -> List Int -> Svg Msg
scatterplot pointData numOfStackedPointsList =
    let
        data =
            pointData.data

        xValues : List Float
        xValues =
            List.map .x data

        yValues : List Float
        yValues =
            List.map .y data

        xScale : ContinuousScale Float
        xScale =
            xValues
                |> Statistics.extent
                |> Maybe.withDefault ( 100.0, 200.0 )
                |> Scale.linear ( 0, w )

        yScale : ContinuousScale Float
        yScale =
            yValues
                |> Statistics.extent
                |> Maybe.withDefault ( 100.0, 200.0 )
                |> Scale.linear ( h, 0 )
    in
    svg
        [ viewBox 0 0 (w + 4 * xPlotPadding) (h + 4 * yPlotPadding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Px 2500 -- Tested with 2560 x 1440 resolution
        , TypedSvg.Attributes.height <| TypedSvg.Types.Px 1000
        , TypedSvg.Attributes.preserveAspectRatio (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin) TypedSvg.Types.Slice
        ]
        [ style []
            [ TypedSvg.Core.text """
             .point text { display: none; }
             .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
             .point:hover text { display: inline; }
             .axis g text {font-size: 23px; display: inline;}
           """ ]
        , g [ class [ "axis" ] ]
            [ g [ transform [ Translate xPlotPadding (h + yPlotPadding) ] ]
                -- X AXIS
                [ Axis.bottom [ Axis.tickCount tickCount ] xScale
                , text_ [ fontFamily [ "sans-serif" ], fontSize fontSizeCaptionsInPx, x (w / 2), y 50 ]
                    [ text
                        (adjustLabels pointData.xDescription)
                    ]
                ]

            -- Y AXIS
            , g [ transform [ Translate xPlotPadding yPlotPadding ] ]
                [ Axis.left [ Axis.tickCount tickCount ] yScale
                , text_
                    [ fontFamily [ "sans-serif" ]
                    , fontSize fontSizeCaptionsInPx
                    , x
                        (if pointData.yDescription == "Smoking status" then
                            50

                         else
                            0
                        )
                    , y -30
                    , textAnchor AnchorMiddle
                    ]
                    [ text (adjustLabels pointData.yDescription) ]
                ]
            ]

        -- RENDER POINTS
        , g [ transform [ Translate xPlotPadding yPlotPadding ] ]
            (List.map2 (pointFunc xScale yScale pointData.xDescription pointData.yDescription) data numOfStackedPointsList)
        ]


pointFunc : ContinuousScale Float -> ContinuousScale Float -> String -> String -> Point -> Int -> Svg Msg
pointFunc scaleX scaleY xDescr yDescr xyPoint numOfStackedPoints =
    g [ class [ "point" ], fontSize <| fontSizeHoverInPx, fontFamily [ "sans-serif" ] ]
        [ circle
            [ cx (Scale.convert scaleX xyPoint.x)
            , cy (Scale.convert scaleY xyPoint.y)
            , r (4 + toFloat numOfStackedPoints)
            , Html.Attributes.style "stroke" "rgba(0, 0, 0,0.4)"
            , Html.Attributes.style "fill" ("rgba(255, " ++ String.fromInt (255 - (numOfStackedPoints * 5)) ++ ", " ++ String.fromInt (255 - (numOfStackedPoints * 5)) ++ ", 0.3)")
            , Html.Events.onClick <| IconsWithXY xDescr yDescr
            ]
            []
        , text_
            [ x (Scale.convert scaleX xyPoint.x)
            , y (Scale.convert scaleY xyPoint.y - 10)
            , textAnchor TypedSvg.Types.AnchorMiddle
            ]
            [ text xyPoint.pointName
            ]
        ]


listsToPointData : List Float -> List Float -> String -> String -> ( PointData, List Int )
listsToPointData listX listY nameX nameY =
    let
        xyTupleList =
            List.map2 (\x y -> ( x, y )) listX listY

        stackedList =
            stacking xyTupleList Dict.empty

        pointsList =
            List.map
                (\tup ->
                    ( Point
                        ("("
                            ++ nameX
                            ++ ": "
                            ++ String.fromFloat (tup |> Tuple.first |> Tuple.first)
                            ++ ", "
                            ++ nameY
                            ++ ": "
                            ++ String.fromFloat (tup |> Tuple.first |> Tuple.second)
                            ++ ") stacked Points: "
                            ++ String.fromInt (tup |> Tuple.second)
                        )
                        (tup |> Tuple.first |> Tuple.first)
                        (tup |> Tuple.first |> Tuple.second)
                    , tup |> Tuple.second
                    )
                )
                stackedList
    in
    ( PointData nameX nameY (List.map Tuple.first pointsList), List.map Tuple.second pointsList )


stacking : List comparable -> Dict comparable Int -> List ( comparable, Int )
stacking list dict =
    case list of
        [] ->
            Dict.toList dict

        first :: rest ->
            let
                count =
                    case Dict.get first dict of
                        Just value ->
                            value + 1

                        Nothing ->
                            1
            in
            Dict.insert first count dict
                |> stacking rest


parallelCoodinatesPlot : MultiDimData -> Svg Msg
parallelCoodinatesPlot multiDimData =
    let
        multiDimPoints =
            multiDimData.data

        pointValues =
            List.map .value multiDimPoints

        numberOfAttributes =
            List.head pointValues |> Maybe.withDefault [] |> List.length

        indexNumberList =
            List.range 0 (numberOfAttributes - 1)

        xList =
            indexNumberList |> List.map (toFloat >> (\x -> x * gapBetweenVerticales))

        scaleList =
            List.map (\indexNum -> scaleIndexNum pointValues indexNum numberOfAttributes) indexNumberList

        transposedPointsList =
            pointValues |> List.Extra.transpose

        yTransposedList =
            List.map2
                (\pointsList scaleIndex -> List.map (\val -> Scale.convert scaleIndex val) pointsList)
                transposedPointsList
                scaleList

        yList =
            List.Extra.transpose yTransposedList

        paths : List (List ( Float, Float ))
        paths =
            List.map (\li -> List.map2 (\x y -> ( x, y )) xList li) yList

        line : List ( Float, Float ) -> Path
        line list =
            List.map (\( x, y ) -> Just <| ( x, y )) list
                |> Shape.line Shape.linearCurve

        lines =
            List.map
                (\list ->
                    g [ transform [ Translate xPlotPadding yPlotPadding ] ]
                        [ Path.element (line list)
                            [ stroke <| Paint <| Color.black
                            , strokeWidth (Mm 0.5)
                            , fill PaintNone
                            ]
                        ]
                )
                paths

        verticalLines : Int -> Svg Msg
        verticalLines num =
            let
                xValOfVerticalLine =
                    List.Extra.getAt num xList |> Maybe.withDefault 42.0

                scaleVal =
                    List.Extra.getAt num scaleList |> Maybe.withDefault (Scale.linear ( h, 0 ) ( 100.0, 200.0 ))
            in
            g [ transform [ Translate (xPlotPadding + xValOfVerticalLine) yPlotPadding ] ]
                [ Axis.left [ Axis.tickCount tickCount ] scaleVal
                , text_ [ fontFamily [ "sans-serif" ], fontSize fontSizeParallelCoordInPx, x -40, y -60, textAnchor AnchorMiddle, Html.Events.onClick <| ChangeDimensions (modBy numberOfAttributes (num - 1)) num ] [ text (String.fromChar '←') ]
                , text_
                    [ fontFamily [ "sans-serif" ]
                    , fontSize fontSizeParallelCoordInPx
                    , x 0
                    , y -30
                    , textAnchor AnchorMiddle
                    , Html.Events.onClick <|
                        ScatterPlotWithXY (Maybe.withDefault "Age" (List.Extra.getAt num multiDimData.dimDescription)) (Maybe.withDefault "Age" (List.Extra.getAt (modBy numberOfAttributes (num + 1)) multiDimData.dimDescription))
                    ]
                    [ text (Maybe.withDefault "Default" (List.Extra.getAt num multiDimData.dimDescription))
                    ]
                , text_ [ fontFamily [ "sans-serif" ], fontSize fontSizeParallelCoordInPx, x 40, y -60, textAnchor AnchorMiddle, Html.Events.onClick <| ChangeDimensions num (modBy numberOfAttributes (num + 1)) ] [ text (String.fromChar '→') ]
                ]
    in
    svg
        [ TypedSvg.Attributes.style "background: white"
        , viewBox 70 0 (2 * w + xPlotPadding) (h + 2 * yPlotPadding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Px 2550 -- Tested with 2560 x 1440 resolution
        , TypedSvg.Attributes.height <| TypedSvg.Types.Px 650
        ]
        [ style []
            [ text
                (".axis g g text {font-size: 15px; display: none;}"
                    ++ ".axis:hover g g text {font-size: 25px; display: inline;}"
                )
            ]
        , g [ class [ "axis" ] ]
            (List.map (\num -> verticalLines num) indexNumberList
                ++ lines
            )
        ]


listsToMultiDimData : List (List Float) -> List String -> MultiDimData
listsToMultiDimData listOfPointVals nameOfDims =
    let
        multiDimPointsList =
            List.map (\li -> MultiDimPoint "" li) listOfPointVals
    in
    MultiDimData nameOfDims multiDimPointsList


scaleIndexNum : List (List Float) -> Int -> Int -> ContinuousScale Float
scaleIndexNum pointVals indexNum numberOfAttr =
    List.Extra.transpose pointVals
        |> List.Extra.getAt indexNum
        |> Maybe.withDefault (List.map toFloat (List.Extra.initialize numberOfAttr (\el -> el * 10)))
        -- [ 0.0, 10.0, 20.0, 30.0,... ]
        |> Statistics.extent
        |> Maybe.withDefault ( 100.0, 200.0 )
        |> Scale.linear ( h, 0 )


iconListsToMultiDimData : List (List Float) -> List String -> MultiDimData
iconListsToMultiDimData listOfDims nameOfDims =
    let
        -- listOfDims is a list of a few very long lists. Each of them contains all values of an attribute.
        -- listOfPointVals is a list of very many short lists. Each of them represents a multidimensional point.
        listOfPointVals =
            List.Extra.transpose listOfDims

        multiDimPointsList =
            List.map (\li -> MultiDimPoint "" li) listOfPointVals
    in
    MultiDimData nameOfDims multiDimPointsList


iconsPlot : MultiDimData -> Html Msg
iconsPlot multiDimData =
    let
        data =
            multiDimData.data

        getAllValsOfOneDim : Int -> List Float
        getAllValsOfOneDim num =
            List.map .value data |> List.Extra.transpose |> List.Extra.getAt num |> Maybe.withDefault []

        xValues =
            getAllValsOfOneDim 0

        yValues =
            getAllValsOfOneDim 1

        stick1Values =
            getAllValsOfOneDim 2

        stick2Values =
            getAllValsOfOneDim 3

        stick3Values =
            getAllValsOfOneDim 4

        stick4Values =
            getAllValsOfOneDim 5

        stick5Values =
            getAllValsOfOneDim 6

        xScale : ContinuousScale Float
        xScale =
            xValues
                |> Statistics.extent
                |> Maybe.withDefault ( 100.0, 200.0 )
                |> Scale.linear ( 0, w )

        yScale : ContinuousScale Float
        yScale =
            yValues
                |> Statistics.extent
                |> Maybe.withDefault ( 100.0, 200.0 )
                |> Scale.linear ( h, 0 )

        stick1Scale : ContinuousScale Float
        stick1Scale =
            angleScale stick1Values

        stick2Scale : ContinuousScale Float
        stick2Scale =
            angleScaleUpLeft stick2Values

        stick3Scale : ContinuousScale Float
        stick3Scale =
            angleScaleUpRight stick3Values

        stick4Scale : ContinuousScale Float
        stick4Scale =
            angleScaleDownRight stick4Values

        stick5Scale : ContinuousScale Float
        stick5Scale =
            angleScaleDownLeft stick5Values

        scaleList =
            [ xScale, yScale, stick1Scale, stick2Scale, stick3Scale, stick4Scale, stick5Scale ]
    in
    svg
        [ viewBox 0 0 (w + 4 * xPlotPadding) (h + 4 * (yPlotPadding + 100))
        , TypedSvg.Attributes.width <| TypedSvg.Types.Px 2500 -- Tested with 2560 x 1440 resolution
        , TypedSvg.Attributes.height <| TypedSvg.Types.Px 1440
        ]
        [ style []
            [ TypedSvg.Core.text """
            .stick text { display: none; }
            .stick:hover text { display: inline; }
            .axis g text {font-size: 23px; display: inline;}
            .stick:hover { stroke-width: 1.5mm; }
            .axis text {font-size: 23px; display: inline;}
           """ ]
        , g [ class [ "axis" ] ]
            [ g [ transform [ Translate xPlotPadding (h + (yPlotPadding + 100)) ] ]
                -- X AXIS
                [ Axis.bottom [ Axis.tickCount tickCount ] xScale
                , text_ [ fontFamily [ "sans-serif" ], fontSize fontSizeCaptionsInPx, x (w / 2), y 50 ]
                    [ text
                        (multiDimData.dimDescription |> List.Extra.getAt 0 |> Maybe.withDefault "" |> adjustLabels)
                    ]
                ]

            -- Y AXIS
            , g [ transform [ Translate xPlotPadding (yPlotPadding + 100) ] ]
                [ Axis.left [ Axis.tickCount tickCount ] yScale
                , text_
                    [ fontFamily [ "sans-serif" ]
                    , fontSize fontSizeCaptionsInPx
                    , x
                        (if (multiDimData.dimDescription |> List.Extra.getAt 1 |> Maybe.withDefault "") == "Smoking status" then
                            50

                         else
                            0
                        )
                    , y -30
                    , textAnchor AnchorMiddle
                    ]
                    [ text
                        (multiDimData.dimDescription |> List.Extra.getAt 1 |> Maybe.withDefault "" |> adjustLabels)
                    ]
                ]
            ]

        -- RENDER STICKFIGURES
        , g [ transform [ Translate xPlotPadding (yPlotPadding + 100) ], Html.Events.onClick <| ParallelCoordinates ]
            (List.map (drawStickFigure scaleList multiDimData.dimDescription) data)
        ]


angleScale : List Float -> ContinuousScale Float
angleScale values =
    Scale.linear ( -0.25 * pi, -0.75 * pi ) (wideAngleExtent values)


angleScaleUpRight : List Float -> ContinuousScale Float
angleScaleUpRight values =
    Scale.linear ( 0, -0.5 * pi ) (wideAngleExtent values)


angleScaleUpLeft : List Float -> ContinuousScale Float
angleScaleUpLeft values =
    Scale.linear ( -0.5 * pi, -pi ) (wideAngleExtent values)


angleScaleDownRight : List Float -> ContinuousScale Float
angleScaleDownRight values =
    Scale.linear ( 0.5 * pi, 0 ) (wideAngleExtent values)


angleScaleDownLeft : List Float -> ContinuousScale Float
angleScaleDownLeft values =
    Scale.linear ( pi, 0.5 * pi ) (wideAngleExtent values)


wideAngleExtent : List Float -> ( Float, Float )
wideAngleExtent values =
    values
        |> Statistics.extent
        |> Maybe.withDefault ( -0.5, 0.5 )


drawStickFigure : List (ContinuousScale Float) -> List String -> MultiDimPoint -> Svg msg
drawStickFigure scaleList dimDescription multiDimPoint =
    let
        numberOfDims =
            List.length dimDescription

        length =
            20

        convertScale : Int -> Float
        convertScale num =
            Scale.convert
                (scaleList |> List.Extra.getAt num |> Maybe.withDefault (Scale.linear ( 0, 0 ) ( 0, 0 )))
                (multiDimPoint.value |> List.Extra.getAt num |> Maybe.withDefault 0.0)

        angle1 =
            convertScale 2

        angle2 =
            convertScale 3

        angle3 =
            convertScale 4

        angle4 =
            convertScale 5

        angle5 =
            convertScale 6

        x1Pos =
            convertScale 0

        y1Pos =
            convertScale 1

        x2Pos =
            x1Pos + (length * cos angle1)

        y2Pos =
            y1Pos + (length * sin angle1)

        x3Pos =
            x2Pos + (length * cos angle2)

        y3Pos =
            y2Pos + (length * sin angle2)

        x4Pos =
            x2Pos + (length * cos angle3)

        y4Pos =
            y2Pos + (length * sin angle3)

        x5Pos =
            x1Pos + (length * cos angle4)

        y5Pos =
            y1Pos + (length * sin angle4)

        x6Pos =
            x1Pos + (length * cos angle5)

        y6Pos =
            y1Pos + (length * sin angle5)

        stickFigureText : Int -> Svg msg
        stickFigureText num =
            text_
                [ x x1Pos
                , y (y1Pos - (40 + toFloat ((numberOfDims - 1) - num) * 20))
                , textAnchor TypedSvg.Types.AnchorMiddle
                ]
                [ text
                    ((dimDescription |> List.Extra.getAt num |> Maybe.withDefault "")
                        ++ ": "
                        ++ (multiDimPoint.value |> List.Extra.getAt num |> Maybe.withDefault 0.0 |> String.fromFloat)
                        ++ (if num == 0 then
                                ""

                            else
                                ", "
                           )
                    )
                ]
    in
    g [ class [ "stick" ], fontSize <| fontSizeHoverInPx, fontFamily [ "sans-serif" ], strokeWidth (Mm 0.5) ]
        [ line [ x1 x1Pos, y1 y1Pos, x2 x2Pos, y2 y2Pos, stroke (Paint (stickLimbColors |> List.Extra.getAt 0 |> Maybe.withDefault Color.black)) ] []
        , line [ x1 x2Pos, y1 y2Pos, x2 x3Pos, y2 y3Pos, stroke (Paint (stickLimbColors |> List.Extra.getAt 1 |> Maybe.withDefault Color.black)) ] []
        , line [ x1 x2Pos, y1 y2Pos, x2 x4Pos, y2 y4Pos, stroke (Paint (stickLimbColors |> List.Extra.getAt 2 |> Maybe.withDefault Color.black)) ] []
        , line [ x1 x1Pos, y1 y1Pos, x2 x5Pos, y2 y5Pos, stroke (Paint (stickLimbColors |> List.Extra.getAt 3 |> Maybe.withDefault Color.black)) ] []
        , line [ x1 x1Pos, y1 y1Pos, x2 x6Pos, y2 y6Pos, stroke (Paint (stickLimbColors |> List.Extra.getAt 4 |> Maybe.withDefault Color.black)) ] []
        , stickFigureText 0
        , stickFigureText 1
        , stickFigureText 2
        , stickFigureText 3
        , stickFigureText 4
        , stickFigureText 5
        , stickFigureText 6
        ]
