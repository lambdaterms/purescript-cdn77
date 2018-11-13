module Request where

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
import Utils (urlEncoded)

apiUrl ∷ String
apiUrl = "https://api.cdn77.com/v2.0"

get ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
get endpoint params = AffJax.get ResponseFormat.json $ apiUrl <> endpoint <> "?" <> encode (urlEncoded params)

post ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
post endpoint params = AffJax.post ResponseFormat.json uri body
  where
    uri = (apiUrl <> endpoint)
    body = (RequestBody.formURLEncoded $ urlEncoded params)

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
