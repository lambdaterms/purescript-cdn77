module Common where

import Affjax (Response, ResponseFormatError)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (ExceptT(..), except)
import Data.Argonaut.Core (Json, caseJsonObject, isNull, stringify)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (const, ($))
import Data.Functor ((<#>), (<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Aff (Aff, attempt)
import Foreign (Foreign)
import Foreign.Object (Object, lookup)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Types (ApiCallError(..), ApiResponse)
import Unsafe.Coerce (unsafeCoerce)
import Utils (coerceJsonHelperImpl, errorType)


readJson ∷ ∀ a. ReadForeign a ⇒ Json → Either ApiCallError a
readJson = errorType (ReturnedObjectTypeError <<< show ) <<< read <<< coerceJson where
    -- | [XXX][WARNING] Assuming that both Json and Foreign represents raw Javascript object, which lies with the foundations of both libraries but may be a subject to change.
    coerceJson ∷ Json → Foreign
    coerceJson = coerceJsonHelperImpl <<< unsafeCoerce

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
        status ← withError readStatusError (stringify <$> lookup "status" obj)
        if status /= "ok"
          then do
            desc ← withError
              (ResourceError $ "Response status was" <> status)
              (stringify <$> lookup "description" obj)
            let errorDetails = case stringify <$> lookup "errors" obj of
                  Nothing -> ""
                  Just errs -> " Errors: " <> errs
            Left $ ResourceError (desc <> errorDetails)
          else pure obj)
    body
  where
    readStatusError = ServerReponseError "Can't read response status"
    readCdnResourceError t = ServerReponseError $ "Can't find " <> t <> " in response."

    withError ∷ ∀ a. ApiCallError → Maybe a → Either ApiCallError a
    withError err = case _ of
      Nothing → Left err
      Just a  → Right a

-- | Reads non-standard response i.e. response that instead of containing data in custom response object,
-- | contains it directly in its body.
readNonStandardResponse ∷ ∀ a. ReadForeign (ApiResponse a) ⇒ Aff (Response (Either ResponseFormatError Json)) → ExceptT ApiCallError Aff (ApiResponse a)
readNonStandardResponse reqAff = readStandardResponse reqAff >>= except <<< readJson <<< coerceJson
  where
    coerceJson :: Object Json → Json
    coerceJson = unsafeCoerce

readResponsesOptionalCustomObject ∷ ∀ a. ReadForeign a ⇒ String → a → Aff (Response (Either ResponseFormatError Json)) → ExceptT ApiCallError Aff a
readResponsesOptionalCustomObject target def reqAff =
  readStandardResponse reqAff <#> lookup target >>= case _ of
    Nothing → pure def
    Just obj → case isNull obj of
      true → pure def
      false → except $ readJson obj

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
