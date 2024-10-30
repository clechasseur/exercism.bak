module RunLengthEncoding exposing (decode, encode)


encode : String -> String
encode string =
    let
        encodeChar : Int -> Char -> String
        encodeChar count c =
            let
                countString =
                    case count of
                        1 -> ""
                        _ -> String.fromInt count
            in
                countString ++ String.fromChar c

        tailrecEncode : (Char, Int) -> List Char -> String -> String
        tailrecEncode (prev, count) input soFar =
            case input of
                [] ->
                    soFar ++ encodeChar count prev
                c::t ->
                    if c == prev then
                        tailrecEncode (c, count + 1) t soFar
                    else
                        tailrecEncode (c, 1) t (soFar ++ encodeChar count prev)
    in
        case String.toList string of
            [] ->
                ""
            c::t ->
                tailrecEncode (c, 1) t ""


decode : String -> String
decode string =
    let
        digitValue : Char -> Int
        digitValue d =
            Char.toCode d - Char.toCode '0'

        decoded : Int -> Char -> String
        decoded count c =
            String.repeat (max count 1) (String.fromChar c)
    
        tailrecDecode : Int -> List Char -> String -> String
        tailrecDecode count input soFar =
            case input of
                [] ->
                    soFar
                c::t ->
                    if Char.isDigit c then
                        tailrecDecode (count * 10 + (digitValue c)) t soFar
                    else
                        tailrecDecode 0 t (soFar ++ decoded count c)
    in
        tailrecDecode 0 (String.toList string) ""
