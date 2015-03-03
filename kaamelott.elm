import Char
import Graphics.Element (..)
import Http
import Maybe
import Signal
import String
import Text
import Graphics.Element (..)
import Mouse
import Text (asText)
import Json.Decode
import Result
import Json.Decode((:=))
import Color (..)
import Window

main : Signal Element
main = Signal.map2 view Window.dimensions responses

view : (Int, Int) -> Http.Response String -> Element
view (x, y) response =
  color darkCharcoal <|
    container x y middle <|
        flow down [display x y response]

decodeJson : String -> String
decodeJson str =
  let obj = Json.Decode.object2 (String.append)
          ("quote" := Json.Decode.string)
          ("meta" := Json.Decode.string) in
  case Json.Decode.decodeString obj str of
    Result.Ok s -> s
    Result.Err l -> l

display : Int -> Int -> Http.Response String -> Element
display x y response =
  let viewText str =
        Text.fromString str
          |> Text.color lightGrey
          |> Text.centered
          |> width (x // 2)
          |> container (x // 2) (y // 2) middle in
  case response of
    Http.Success address -> viewText (decodeJson address)
    Http.Waiting -> image 16 16 "waiting.gif"
    Http.Failure _ _ -> Text.asText response

responses : Signal (Http.Response String)
responses =
  Http.sendGet (Signal.subscribe toUrl)

toUrl : Signal.Channel String
toUrl = Signal.channel ("http://kaamelott.cha.moe")
