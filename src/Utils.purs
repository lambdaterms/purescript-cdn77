module Utils where

import Affjax (Response, ResponseFormatError)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (ExceptT(..), except)
import Data.Argonaut.Core (Json, caseJsonObject, toString)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.FormURLEncoded (FormURLEncoded)
import Data.Function (const, ($))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, attempt)
import Foreign (Foreign)
import Foreign.Object (Object, lookup)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write, writeImpl)
import Types (ApiCallError(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import urlEncodedImpl ∷ (String → String → Tuple String (Maybe String)) →  Foreign → Array (Tuple String (Maybe String))

urlEncoded ∷ ∀ r. WriteForeign { | r } ⇒ { | r } → FormURLEncoded
urlEncoded = wrap <<< urlEncodedImpl buildTuple <<< writeImpl
  where
    buildTuple k v = Tuple k (Just v)


readJson ∷ ∀ a. ReadForeign a ⇒ Json → Either ApiCallError a
readJson = errorType (ServerReponseError <<< show ) <<< read <<< coerceJson
  where
    -- | [XXX][WARNING] Assuming that both Json and Foreign represents raw Javascript object, which lies with the foundations of both libraries but may be a subject to change.
    coerceJson ∷ Json → Foreign
    coerceJson = unsafeCoerce

writeJson ∷ ∀ a. WriteForeign a ⇒ a → Json
writeJson = coerceJson <<< write
  where
    -- | [XXX][WARNING] Assuming that both Json and Foreign represents raw Javascript object, which lies with the foundations of both libraries but may be a subject to change.
    coerceJson ∷ Foreign → Json
    coerceJson = unsafeCoerce

-- | Reads through a "standard response" as defined in https://client.cdn77.com/support/api/version/2.0#standard-response,
-- | stopping on the first occured error. If the response is valid and suggests success returns response's body as Object Json
readStandardResponse ∷ Aff (Response (Either ResponseFormatError Json)) → ExceptT ApiCallError Aff (Object Json)
readStandardResponse reqAff = ExceptT $ attempt reqAff >>= \respAttempt → pure $ do
  resp ← errorType (RequestError <<< show) respAttempt
  body ← errorType (const $ ServerReponseError "Response body not found") resp.body
  caseJsonObject
    (Left $ ServerReponseError "Can't parse server's response")
    (\obj → do
        status ← withError readStatusError (lookup "status" obj >>= toString)
        if status /= "ok"
          then do
            desc ← withError
              (ResourceError $ "Response status was" <> status)
              (lookup "description" obj >>= toString)
            Left $ ResourceError desc
          else pure obj)
    body
  where
    readStatusError = ServerReponseError "Can't read response status"
    readCdnResourceError t = ServerReponseError $ "Can't find " <> t <> " in response."

    withError ∷ ∀ a. ApiCallError → Maybe a → Either ApiCallError a
    withError err = case _ of
      Nothing → Left err
      Just a  → Right a

readResponsesCustomObject ∷ ∀ a. ReadForeign a ⇒ String → Aff (Response (Either ResponseFormatError Json)) → ExceptT ApiCallError Aff a
readResponsesCustomObject target reqAff =
  readStandardResponse reqAff >>=
  lookup target >>> withError (readCdnResourceError target) >>> pure >>> ExceptT >>=
  readJson >>> except
  where
    readStatusError = ServerReponseError "Can't read response status"
    readCdnResourceError t = ServerReponseError $ "Can't find " <> t <> " in response."

    withError err = case _ of
      Nothing → Left err
      Just a  → Right a

errorType ∷ ∀ a b c. (a → c) → Either a b → Either c b
errorType f = case _ of
  Left x → Left (f x)
  Right x → Right x
