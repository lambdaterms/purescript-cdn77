module Node.Network.Cdn77.Request where

import Affjax (Response)
import Affjax as AffJax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.FormURLEncoded (encode)
import Data.Function (($))
import Data.Semigroup ((<>))
-- import Debug.Trace (trace)
import Effect.Aff (Aff)
import Simple.JSON (class WriteForeign)
import Node.Network.Cdn77.Utils (urlEncoded)

apiUrl ∷ String
apiUrl = "https://api.cdn77.com/v2.0"

get ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
get endpoint params = AffJax.get ResponseFormat.json $ apiUrl <> endpoint <> "?" <> encode (urlEncoded params)

get2
  ∷ ∀ p1 p2
  . WriteForeign { | p1 }
  ⇒ WriteForeign { | p2 }
  ⇒ String
  → { | p1 }
  → { | p2 } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
get2 endpoint p1 p2 = AffJax.get ResponseFormat.json $ apiUrl <> endpoint <> "?" <> encode (urlEncoded p1 <> urlEncoded p2)

post ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
post endpoint params = AffJax.post ResponseFormat.json uri body
  where
    uri = (apiUrl <> endpoint)
    body = (RequestBody.formURLEncoded $ urlEncoded params)

post2
  ∷ ∀ p2 p1
  . WriteForeign { | p1 }
  ⇒ WriteForeign { | p2 }
  ⇒ String
  → { | p1 }
  → { | p2 }
  → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
post2 endpoint p1 p2 = AffJax.post ResponseFormat.json uri body
  where
    uri = (apiUrl <> endpoint)
    body = (RequestBody.formURLEncoded $ urlEncoded p1 <> urlEncoded p2)

------------- debug traces
-- get ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
-- get endpoint params = AffJax.get ResponseFormat.json $
--   let
--     u = apiUrl <> endpoint <> "?" <> encode (urlEncoded params)
--   in trace u $ const u

-- post ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
-- post endpoint params =
--   trace uri $ const $ trace body $ const $ AffJax.post ResponseFormat.json uri body
--   where
--     uri = (apiUrl <> endpoint)
--     body = (RequestBody.formURLEncoded $ urlEncoded params)
