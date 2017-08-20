module BigDecimal exposing (..)

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

toBigDecimal : String -> Maybe BigDecimal
toBigDecimal s =
    let parts = String.split "." s
        ints = List.map String.toInt parts
    in
        case ints of
            [] -> Nothing
            [integerPart] -> case integerPart of
                Err msg -> Nothing
                Ok integerPart -> Just (newRecord integerPart Nothing)
            [integerPart, fractionalPart] -> case integerPart of
                Err msg -> case fractionalPart of
                    Err msg -> Nothing
                    Ok fractionalPart -> Just (newRecord 0 (Just fractionalPart))
                Ok integerPart -> case fractionalPart of
                    Err msg -> Just (newRecord integerPart Nothing)
                    Ok fractionalPart -> Just (newRecord integerPart (Just fractionalPart))
            _ -> Nothing

stripTrailingZeros : Maybe BigDecimal -> Maybe BigDecimal
stripTrailingZeros bd =
    case bd of
        Just bd -> case bd.fractionalPart of
                       Just val ->
                           let
                               newInt = toString val |> String.toList |> List.filter (\n -> n /= '0') |> String.fromList |> String.toInt
                           in
                               case newInt of
                                   Err msg -> Just bd
                                   Ok newFractionalPart -> Just (updateFractionalPart newFractionalPart bd)
                       Nothing -> Just bd
        Nothing -> Nothing
