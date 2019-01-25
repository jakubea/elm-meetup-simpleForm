module Main exposing (..)

import Browser
import Css
import Focus
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events



---- MODEL ----


type alias Model =
    { billingAddress : Address
    , deliveryAddress : Address
    }


type alias Address =
    { firstName : String
    , lastName : String
    , street : String
    , zip : String
    , city : String
    }


init : ( Model, Cmd Msg )
init =
    ( { billingAddress = Address "" "" "" "" ""
      , deliveryAddress = Address "" "" "" "" ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdatedBillingAddress AddressMsg
    | UpdatedDeliveryAddress AddressMsg


type AddressMsg
    = InsertedFirstName String
    | InsertedLastName String
    | InsertedStreet String
    | InsertedZip String
    | InsertedCity String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedBillingAddress subMsg ->
            ( { model | billingAddress = updateAddress subMsg model.billingAddress }, Cmd.none )

        UpdatedDeliveryAddress subMsg ->
            ( { model | deliveryAddress = updateAddress subMsg model.deliveryAddress }, Cmd.none )


updateAddress : AddressMsg -> Address -> Address
updateAddress msg address =
    case msg of
        InsertedFirstName firstName ->
            { address | firstName = firstName }

        InsertedLastName lastName ->
            { address | lastName = lastName }

        InsertedStreet street ->
            { address | street = street }

        InsertedZip zip ->
            { address | zip = zip }

        InsertedCity city ->
            { address | city = city }



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ rowView
            [ addressFormView "Billing address" model.billingAddress UpdatedBillingAddress
            , addressFormView "Delivery address" model.deliveryAddress UpdatedDeliveryAddress
            ]
        , rowView
            [ addressView "Billing address" model.billingAddress
            , addressView "Delivery address" model.deliveryAddress
            ]
        ]


inputView : (AddressMsg -> Msg) -> ( String, String, String -> AddressMsg ) -> Html Msg
inputView wrapMsg ( text, value, msg ) =
    Html.div
        [ Attributes.css
            [ Css.padding <| Css.px 5
            , Css.width <| Css.pct 100
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceBetween
            ]
        ]
        [ Html.label [] [ Html.text <| text ++ ": " ]
        , Html.input [ Events.onInput msg, Attributes.value value ] []
        ]
        |> Html.map wrapMsg


addressFormView : String -> Address -> (AddressMsg -> Msg) -> Html Msg
addressFormView heading { firstName, lastName, street, zip, city } wrapMsg =
    columnView <|
        Html.h3 [ Attributes.css [ Css.alignSelf Css.center ] ] [ Html.text heading ]
            :: List.map (inputView wrapMsg)
                [ ( "First name", firstName, InsertedFirstName )
                , ( "Last name", lastName, InsertedLastName )
                , ( "Street", street, InsertedStreet )
                , ( "Zip", zip, InsertedZip )
                , ( "City", city, InsertedCity )
                ]


addressView : String -> Address -> Html Msg
addressView heading address =
    columnView <|
        Html.h3 [] [ Html.text heading ]
            :: List.map labelAndValueView
                [ ( "First name: ", address.firstName )
                , ( "Last name: ", address.lastName )
                , ( "Street: ", address.street )
                , ( "ZIP: ", address.zip )
                , ( "City: ", address.city )
                ]


labelAndValueView : ( String, String ) -> Html Msg
labelAndValueView ( label, value ) =
    Html.span []
        [ Html.span [ Attributes.css [ Css.fontWeight Css.bold ] ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


rowView : List (Html Msg) -> Html Msg
rowView =
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceAround
            ]
        ]


columnView : List (Html Msg) -> Html Msg
columnView =
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.alignItems Css.flexStart
            , Css.flexDirection Css.column
            , Css.padding <| Css.px 20
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> Html.toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
