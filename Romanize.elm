module Components.Romanize exposing (standard)

import Array



-- The largest number you can write in Roman numerals is 3,999 which is MMMCMXCIX.
-- You can represent numbers larger than 3,999 in Roman numerals using an overline.
-- An overline on a Roman numeral means you are multiplying that Roman numeral by 1,000.
-- For the number 50,000 in Roman numerals you would use the Roman numeral L (50) with an overline to make it 50,000.
-- Furey, Edward "Roman Numeral Converter" at https://www.calculatorsoup.com/calculators/conversions/roman-numeral-converter.php


standard : Int -> String
standard int =
    -- Standard will only accept an integer between 1 and 3,999
    if int <= 0 || int >= 4000 then
        "Romanize | Integer out of bounds: " ++ String.fromInt int

    else
        String.join ""
            -- Combine each roman numeral into one String
            (List.reverse
                -- Reverse the list
                (Array.toList
                    -- Convert array to list
                    (Array.indexedMap
                        -- Set each index of the array to their appropriate roman numeral
                        (\index item ->
                            case index of
                                0 ->
                                    if item <= 3 && item >= 1 then
                                        String.repeat item "I"

                                    else if item == 4 then
                                        "IV"

                                    else if item == 5 then
                                        "V"

                                    else if item <= 8 && item >= 6 then
                                        "V" ++ String.repeat (item - 5) "I"

                                    else if item == 9 then
                                        "IX"

                                    else
                                        ""

                                1 ->
                                    if item <= 3 && item >= 1 then
                                        String.repeat item "X"

                                    else if item == 4 then
                                        "XL"

                                    else if item == 5 then
                                        "L"

                                    else if item <= 8 && item >= 6 then
                                        "L" ++ String.repeat (item - 5) "X"

                                    else if item == 9 then
                                        "XC"

                                    else
                                        ""

                                2 ->
                                    if item <= 3 && item >= 1 then
                                        String.repeat item "C"

                                    else if item == 4 then
                                        "CD"

                                    else if item == 5 then
                                        "D"

                                    else if item <= 8 && item >= 6 then
                                        "D" ++ String.repeat (item - 5) "C"

                                    else if item == 9 then
                                        "CM"

                                    else
                                        ""

                                3 ->
                                    String.repeat item "M"

                                _ ->
                                    ""
                        )
                        (Array.fromList
                            -- Convert Int to String -> Split into List -> map each back to an integer -> reverse List -> convert to Array
                            (List.reverse
                                (List.map
                                    (\s ->
                                        Maybe.withDefault
                                            0
                                            (String.toInt s)
                                    )
                                    (String.split
                                        ""
                                        (String.fromInt int)
                                    )
                                )
                            )
                        )
                    )
                )
            )
