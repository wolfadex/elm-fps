port module Main exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Array2d exposing (Array2d)
import Axis3d
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Length exposing (Length, Meters)
import Pixels
import Point3d exposing (Point3d)
import Quantity
import Scene3d
import Scene3d.Material
import Set exposing (Set)
import Speed exposing (Speed)
import Task
import Util.Function
import Viewpoint3d
import WebGL.Texture


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { pointerLocked : Bool
    , keyStates : Set String
    , look : { xy : Angle, z : Angle }
    , eyePoint : Point3d Meters WorldCoordinates
    , speed : Speed
    , sprintingSpeed : Speed
    , scene :
        List
            { entity : Scene3d.Entity WorldCoordinates
            , boundingBox : Maybe (BoundingBox3d Meters WorldCoordinates)
            }
    }


type WorldCoordinates
    = WorldCoordinates Never


type Tile
    = Open
    | Solid


level0 : Array2d Tile
level0 =
    Array2d.fromList
        [ [ Open, Open, Open, Open ]
        , [ Open, Solid, Open, Open ]
        , [ Open, Open, Open, Open ]
        , [ Open, Open, Open, Open ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pointerLocked = False
      , keyStates = Set.empty
      , look =
            { xy = Angle.degrees 225
            , z = Angle.degrees 0
            }
      , eyePoint = Point3d.meters 13 13 1.8
      , speed = Speed.metersPerSecond 10
      , sprintingSpeed = Speed.metersPerSecond 20
      , scene = []
      }
    , Scene3d.Material.load "assets/blockout-textures-master/png/128/white_check.png"
        |> Task.attempt TextureLoaded
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pointerLocked PointedLocked
        , if model.pointerLocked then
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Tick
                , Browser.Events.onMouseMove decodePointerMove
                , Browser.Events.onKeyDown decodeKeyDown
                , Browser.Events.onKeyUp decodeKeyUp
                ]

          else
            Sub.none
        ]


port pointerLocked : (Bool -> msg) -> Sub msg


decodePointerMove : Json.Decode.Decoder Msg
decodePointerMove =
    Json.Decode.map2 PointerMoved
        (Json.Decode.field "movementX" Json.Decode.int)
        (Json.Decode.field "movementY" Json.Decode.int)


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map KeyDown
        (Json.Decode.field "key" Json.Decode.string)


decodeKeyUp : Json.Decode.Decoder Msg
decodeKeyUp =
    Json.Decode.map KeyUp
        (Json.Decode.field "key" Json.Decode.string)


type Msg
    = Tick Float
    | PointerMoved Int Int
    | PointedLocked Bool
    | KeyDown String
    | KeyUp String
    | TextureLoaded (Result WebGL.Texture.Error (Scene3d.Material.Texture Color))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( { model
                | eyePoint =
                    let
                        desiredEyePoint =
                            movePlayer deltaMs model

                        zHeight =
                            Point3d.zCoordinate desiredEyePoint
                                |> Quantity.divideBy -2

                        playerBoundingBox =
                            BoundingBox3d.withDimensions
                                ( Length.meters 0.5
                                , Length.meters 0.5
                                , Length.meters 1.8
                                )
                                (desiredEyePoint
                                    |> Point3d.translateIn Direction3d.negativeZ zHeight
                                )

                        collisions =
                            model.scene
                                |> List.filterMap
                                    (\{ boundingBox } ->
                                        case boundingBox of
                                            Just bb ->
                                                BoundingBox3d.intersection bb playerBoundingBox

                                            Nothing ->
                                                Nothing
                                    )
                                |> Debug.log "collisions"

                        finalEyePoint =
                            collisions
                                |> List.foldl
                                    (\boundingBox eyePoint ->
                                        let
                                            ( x, y, _ ) =
                                                BoundingBox3d.dimensions boundingBox
                                        in
                                        eyePoint
                                            |> Point3d.translateIn Direction3d.positiveX y
                                            |> Point3d.translateIn Direction3d.positiveY x
                                    )
                                    desiredEyePoint
                    in
                    -- finalEyePoint
                    case collisions of
                        [] ->
                            desiredEyePoint

                        _ ->
                            model.eyePoint
              }
            , Cmd.none
            )

        TextureLoaded (Err err) ->
            Debug.todo ""

        TextureLoaded (Ok texture) ->
            ( { model
                | scene =
                    level0
                        |> Array.foldl
                            (\row ( acc, columnIndex ) ->
                                ( row
                                    |> Array.foldl
                                        (\tile ( acc_, rowIndex ) ->
                                            case tile of
                                                Open ->
                                                    ( renderOpenTiles texture columnIndex rowIndex
                                                        ++ acc_
                                                    , rowIndex + 1
                                                    )

                                                Solid ->
                                                    ( acc_
                                                    , rowIndex + 1
                                                    )
                                        )
                                        ( acc, 0 )
                                    |> Tuple.first
                                , columnIndex + 1
                                )
                            )
                            ( [], 0 )
                        |> Tuple.first
              }
            , Cmd.none
            )

        PointedLocked locked ->
            ( { model | pointerLocked = locked }, Cmd.none )

        KeyDown key ->
            ( { model
                | keyStates = Set.insert (String.toUpper key) model.keyStates
              }
            , Cmd.none
            )

        KeyUp key ->
            ( { model
                | keyStates = Set.remove (String.toUpper key) model.keyStates
              }
            , Cmd.none
            )

        PointerMoved x y ->
            ( { model
                | look =
                    { xy =
                        model.look.xy
                            |> Quantity.minus (Angle.degrees (toFloat x / 10))
                    , z =
                        model.look.z
                            |> Quantity.minus (Angle.degrees (toFloat y / 10))
                    }
              }
            , Cmd.none
            )


movePlayer : Float -> Model -> Point3d Meters WorldCoordinates
movePlayer deltaMs model =
    let
        distance : Length
        distance =
            (if Set.member "SHIFT" model.keyStates then
                model.sprintingSpeed

             else
                model.speed
            )
                |> Quantity.for (Duration.milliseconds deltaMs)
    in
    model.eyePoint
        |> Util.Function.applyIf (Set.member "W" model.keyStates)
            (Point3d.translateIn
                (Direction3d.xyZ model.look.xy zeroDegress)
                distance
            )
        |> Util.Function.applyIf (Set.member "S" model.keyStates)
            (Point3d.translateIn
                (Direction3d.xyZ
                    (model.look.xy
                        |> Quantity.plus oneHundredEightyDegrees
                    )
                    zeroDegress
                )
                distance
            )
        |> Util.Function.applyIf (Set.member "A" model.keyStates)
            (Point3d.translateIn
                (Direction3d.xyZ
                    (model.look.xy
                        |> Quantity.plus ninetyDegrees
                    )
                    zeroDegress
                )
                distance
            )
        |> Util.Function.applyIf (Set.member "D" model.keyStates)
            (Point3d.translateIn
                (Direction3d.xyZ
                    (model.look.xy
                        |> Quantity.minus ninetyDegrees
                    )
                    zeroDegress
                )
                distance
            )


renderOpenTiles :
    Scene3d.Material.Texture Color
    -> Int
    -> Int
    ->
        List
            { entity : Scene3d.Entity WorldCoordinates
            , boundingBox : Maybe (BoundingBox3d Meters WorldCoordinates)
            }
renderOpenTiles texture columnIndex rowIndex =
    let
        columnPos :
            Maybe
                ( Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
                , Point3d Meters WorldCoordinates
                )
        columnPos =
            case Array2d.get rowIndex (columnIndex + 1) level0 of
                Just Open ->
                    Nothing

                Just Solid ->
                    Just
                        ( wallX
                        , Point3d.meters
                            (toFloat rowIndex * 4)
                            (toFloat (columnIndex + 1) * 4)
                            0
                        )

                Nothing ->
                    Just
                        ( wallX
                        , Point3d.meters
                            (toFloat rowIndex * 4)
                            (toFloat (columnIndex + 1) * 4)
                            0
                        )

        columnNeg :
            Maybe
                ( Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
                , Point3d Meters WorldCoordinates
                )
        columnNeg =
            case Array2d.get rowIndex (columnIndex - 1) level0 of
                Just Open ->
                    Nothing

                Just Solid ->
                    Just
                        ( wallX
                        , Point3d.meters
                            (toFloat rowIndex * 4)
                            (toFloat columnIndex * 4)
                            0
                        )

                Nothing ->
                    Just
                        ( wallX
                        , Point3d.meters
                            (toFloat rowIndex * 4)
                            (toFloat columnIndex * 4)
                            0
                        )

        rowPos :
            Maybe
                ( Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
                , Point3d Meters WorldCoordinates
                )
        rowPos =
            case Array2d.get (rowIndex + 1) columnIndex level0 of
                Just Open ->
                    Nothing

                Just Solid ->
                    Just
                        ( wallY
                        , Point3d.meters
                            (toFloat (rowIndex + 1) * 4)
                            (toFloat columnIndex * 4)
                            0
                        )

                Nothing ->
                    Just
                        ( wallY
                        , Point3d.meters
                            (toFloat (rowIndex + 1) * 4)
                            (toFloat columnIndex * 4)
                            0
                        )

        rowNeg :
            Maybe
                ( Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
                , Point3d Meters WorldCoordinates
                )
        rowNeg =
            case Array2d.get (rowIndex - 1) columnIndex level0 of
                Just Open ->
                    Nothing

                Just Solid ->
                    Just
                        ( wallY
                        , Point3d.meters
                            (toFloat rowIndex * 4)
                            (toFloat columnIndex * 4)
                            0
                        )

                Nothing ->
                    Just
                        ( wallY
                        , Point3d.meters
                            (toFloat rowIndex * 4)
                            (toFloat columnIndex * 4)
                            0
                        )
    in
    List.filterMap identity
        [ Just
            { entity =
                floorTile texture
                    (Point3d.meters
                        (toFloat rowIndex * 4)
                        (toFloat columnIndex * 4)
                        0
                    )
                    |> Tuple.first
            , boundingBox = Nothing
            }
        , Just
            { entity =
                ceilingTile texture
                    (Point3d.meters
                        (toFloat rowIndex * 4)
                        (toFloat columnIndex * 4)
                        0
                    )
                    |> Tuple.first
            , boundingBox = Nothing
            }
        , columnPos
            |> Maybe.map
                (\( f, p ) ->
                    let
                        ( e, otherP ) =
                            f texture p
                    in
                    { entity = e
                    , boundingBox = Just (BoundingBox3d.from p otherP)
                    }
                )
        , columnNeg
            |> Maybe.map
                (\( f, p ) ->
                    let
                        ( e, otherP ) =
                            f texture p
                    in
                    { entity = e
                    , boundingBox = Just (BoundingBox3d.from p otherP)
                    }
                )
        , rowPos
            |> Maybe.map
                (\( f, p ) ->
                    let
                        ( e, otherP ) =
                            f texture p
                    in
                    { entity = e
                    , boundingBox = Just (BoundingBox3d.from p otherP)
                    }
                )
        , rowNeg
            |> Maybe.map
                (\( f, p ) ->
                    let
                        ( e, otherP ) =
                            f texture p
                    in
                    { entity = e
                    , boundingBox = Just (BoundingBox3d.from p otherP)
                    }
                )
        ]


floorTile : Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
floorTile texture position =
    let
        otherP =
            position
                |> Point3d.translateIn Direction3d.positiveX (Length.meters 4)
                |> Point3d.translateIn Direction3d.positiveY (Length.meters 4)
    in
    ( Scene3d.quad
        (Scene3d.Material.texturedMatte texture)
        position
        (position
            |> Point3d.translateIn Direction3d.positiveX (Length.meters 4)
        )
        otherP
        (position
            |> Point3d.translateIn Direction3d.positiveY (Length.meters 4)
        )
    , otherP
    )


ceilingTile : Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
ceilingTile texture position =
    let
        otherP =
            position
                |> Point3d.translateIn Direction3d.positiveX (Length.meters 4)
                |> Point3d.translateIn Direction3d.positiveY (Length.meters 4)
                |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
    in
    ( Scene3d.quad
        (Scene3d.Material.texturedMatte texture)
        (position
            |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
        )
        (position
            |> Point3d.translateIn Direction3d.positiveX (Length.meters 4)
            |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
        )
        otherP
        (position
            |> Point3d.translateIn Direction3d.positiveY (Length.meters 4)
            |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
        )
    , otherP
    )


wallX : Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
wallX texture position =
    let
        otherP =
            position
                |> Point3d.translateIn Direction3d.positiveX (Length.meters 4)
                |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
    in
    ( Scene3d.quad
        (Scene3d.Material.texturedMatte texture)
        position
        (position
            |> Point3d.translateIn Direction3d.positiveX (Length.meters 4)
        )
        otherP
        (position
            |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
        )
    , otherP
    )


wallY : Scene3d.Material.Texture Color -> Point3d Meters WorldCoordinates -> ( Scene3d.Entity WorldCoordinates, Point3d Meters WorldCoordinates )
wallY texture position =
    let
        otherP =
            position
                |> Point3d.translateIn Direction3d.positiveY (Length.meters 4)
                |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
    in
    ( Scene3d.quad
        (Scene3d.Material.texturedMatte texture)
        position
        (position
            |> Point3d.translateIn Direction3d.positiveY (Length.meters 4)
        )
        otherP
        (position
            |> Point3d.translateIn Direction3d.positiveZ (Length.meters 4)
        )
    , otherP
    )


zeroDegress : Angle
zeroDegress =
    Angle.degrees 0


ninetyDegrees : Angle
ninetyDegrees =
    Angle.degrees 90


oneHundredEightyDegrees : Angle
oneHundredEightyDegrees =
    Angle.degrees 180


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Dungeon"
    , body =
        [ view3d model
        ]
    }


view3d : Model -> Html.Html Msg
view3d model =
    Html.div
        [ Html.Attributes.id "game-window"
        , Html.Attributes.style "width" "800px"
        , Html.Attributes.style "height" "600px"
        ]
        [ let
            camera =
                Camera3d.perspective
                    { -- Camera is at the point (4, 2, 2), looking at the point
                      -- (0, 0, 0), oriented so that positive Z appears up
                      viewpoint =
                        Viewpoint3d.lookAt
                            { focalPoint =
                                model.eyePoint
                                    |> Point3d.translateIn (Direction3d.xyZ model.look.xy model.look.z) (Length.meters 1)
                            , eyePoint = model.eyePoint
                            , upDirection = Direction3d.positiveZ
                            }

                    -- The image on the screen will have a total rendered 'height'
                    -- of 30 degrees; small angles make the camera act more like a
                    -- telescope and large numbers make it act more like a fisheye
                    -- lens
                    , verticalFieldOfView = Angle.degrees 30
                    }
          in
          Scene3d.sunny
            { dimensions = ( Pixels.int 800, Pixels.int 600 )
            , camera = camera
            , upDirection = Direction3d.positiveZ
            , sunlightDirection =
                Direction3d.negativeZ
                    |> Direction3d.rotateAround Axis3d.y (Angle.degrees 15)
            , shadows = True
            , clipDepth = Length.cssPixels 1
            , background = Scene3d.backgroundColor Color.blue
            , entities =
                model.scene
                    |> List.map .entity
            }
        ]
