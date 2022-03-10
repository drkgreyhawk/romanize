module Romanize exposing (standardConversion)

import Array exposing (Array)



-- The largest number you can write in Roman numerals is 3,999 which is MMMCMXCIX.
-- You can represent numbers larger than 3,999 in Roman numerals using an overline.
-- An overline on a Roman numeral means you are multiplying that Roman numeral by 1,000.
-- For the number 50,000 in Roman numerals you would use the Roman numeral L (50) with an overline to make it 50,000.
-- Furey, Edward "Roman Numeral Converter" at https://www.calculatorsoup.com/calculators/conversions/roman-numeral-converter.php


{-| This is the standard Roman numeral conversion function and will only accept an integer between 1 and 3,999.

"The largest number you can write in Roman numerals is 3,999 which is MMMCMXCIX." - Furey, Edward "Roman Numeral Converter"
-}

standardConversion : Int -> String
standardConversion int =
    if int <= 0 || int >= 4000 then
        "Romanize | Integer out of bounds: " ++ String.fromInt int

    else
        convertToRomanNumeral int


convertToRomanNumeral : Int -> String
convertToRomanNumeral int =
    String.join ""
        (List.reverse
            (getRomanizedList int)
        )


getRomanizedList : Int -> List String
getRomanizedList int =
    Array.toList
        (Array.indexedMap
            romanizeArrayIndex
            (convertToArray int)
        )


convertToArray : Int -> Array Int
convertToArray int =
    Array.fromList (reversedIntegerList int)


reversedIntegerList : Int -> List Int
reversedIntegerList int =
    List.reverse
        (List.map
            stringToInt
            (intToStringThenSplit int)
        )


intToStringThenSplit : Int -> List String
intToStringThenSplit int =
    String.split "" (String.fromInt int)


stringToInt : String -> Int
stringToInt string =
    Maybe.withDefault 0 (String.toInt string)


romanizeArrayIndex : Int -> Int -> String
romanizeArrayIndex index int =
    case index of
        0 ->
            if int <= 3 && int >= 1 then
                String.repeat int "I"

            else if int == 4 then
                "IV"

            else if int == 5 then
                "V"

            else if int <= 8 && int >= 6 then
                "V" ++ String.repeat (int - 5) "I"

            else if int == 9 then
                "IX"

            else
                ""

        1 ->
            if int <= 3 && int >= 1 then
                String.repeat int "X"

            else if int == 4 then
                "XL"

            else if int == 5 then
                "L"

            else if int <= 8 && int >= 6 then
                "L" ++ String.repeat (int - 5) "X"

            else if int == 9 then
                "XC"

            else
                ""

        2 ->
            if int <= 3 && int >= 1 then
                String.repeat int "C"

            else if int == 4 then
                "CD"

            else if int == 5 then
                "D"

            else if int <= 8 && int >= 6 then
                "D" ++ String.repeat (int - 5) "C"

            else if int == 9 then
                "CM"

            else
                ""

        3 ->
            String.repeat int "M"

        _ ->
            ""
