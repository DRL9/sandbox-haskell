module RealWorldHaskell.C05PrettyJSON (renderJValue)
where


import RealWorldHaskell.C05Prettify (
    Doc,
    text,
    string,
    double,
    series,
    (+++)
    )
import RealWorldHaskell.C05SimpleJSON (JValue(..))

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber a)= double a
renderJValue (JString s) = string s


renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          +++ text ": "
                          +++ renderJValue val
