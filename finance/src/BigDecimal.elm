module BigDecimal exposing (..)

import Regex exposing (HowMany, regex)

type alias Precision = Int
type alias Scale = Int
type alias IntegerPart = Int
type alias FractionalPart = Maybe Int
type alias BigDecimal =
    {integerPart : IntegerPart,
    fractionalPart : FractionalPart,
    precision : Precision,
    scale : Scale}

getScale : Maybe Int -> Int
getScale fractionalPart =
    case fractionalPart of
        Just val -> toString val |> String.length
        Nothing -> 0

getPrecision : Int -> Maybe Int -> Int
getPrecision integerPart fractionalPart =
    case fractionalPart of
            Just val -> (toString val |> String.length) + (toString integerPart |> String.length)
            Nothing -> toString integerPart |> String.length

updateIntegerPart : Int -> BigDecimal -> BigDecimal
updateIntegerPart newIntegerPart bd =
    {bd | integerPart = newIntegerPart
          , precision = getPrecision newIntegerPart bd.fractionalPart}

updateFractionalPart : Int -> BigDecimal -> BigDecimal
updateFractionalPart newFractionalPart bd =
    {bd | fractionalPart = Just newFractionalPart
          , scale = Just newFractionalPart |> getScale
          , precision = getPrecision bd.integerPart (Just newFractionalPart)}

newRecord : Int -> Maybe Int -> BigDecimal
newRecord integerPart fractionalPart =
    BigDecimal integerPart fractionalPart (getPrecision integerPart fractionalPart) (getScale fractionalPart)

toBigDecimal : String -> Result String BigDecimal
toBigDecimal s =
    let parts = String.split "." s
        ints = List.map String.toInt parts
    in
        case ints of
            [] -> Err "No number specified!"
            [integerPart] -> case integerPart of
                Err msg -> Err msg
                Ok integerPart -> Ok (newRecord integerPart Nothing)
            [integerPart, fractionalPart] -> case integerPart of
                Err msg -> case fractionalPart of
                    Err msg -> Err msg
                    Ok fractionalPart -> Ok (newRecord 0 (Just fractionalPart))
                Ok integerPart -> case fractionalPart of
                    Err msg -> Ok (newRecord integerPart Nothing)
                    Ok fractionalPart -> Ok (newRecord integerPart (Just fractionalPart))
            _ -> Err "To many '.' characters!"

stripTrailingZeros : BigDecimal -> BigDecimal
stripTrailingZeros bd =
        case bd.fractionalPart of
            Just val ->
                let
                    newInt = toString val |> Regex.replace Regex.All (regex "[0]+$") (\_ -> "") |> String.toInt
                in
                    case newInt of
                        Err msg -> bd -- do nothing this number looks strange
                        Ok newFractionalPart -> updateFractionalPart newFractionalPart bd
            Nothing -> bd