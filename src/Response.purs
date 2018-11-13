module Response where

import Affjax (Response, ResponseFormatError)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut.Core (Json, caseJsonObject, stringify, toString)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Aff (Aff, attempt)
import Foreign (Foreign)
import Foreign.Object (lookup)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Types (ApiCallError(..), ApiResp)
import Unsafe.Coerce (unsafeCoerce)
import Utils (coerceJsonHelperImpl, errorType)


-- | Reads a "standard response" as defined in https://client.cdn77.com/support/api/version/2.0#standard-response,
-- | stopping on the first occured error. If the response is valid and suggests success returns response's body as Json
-- | [XXX][WARNING] Assuming that both Json and Foreign represents raw Javascript object,
-- | which lies with the foundations of both libraries but may be a subject to change.
readCdn77Response
  ∷ ∀ a. ReadForeign (ApiResp a)
  ⇒ Aff (Response (Either ResponseFormatError Json))
  → ExceptT ApiCallError Aff (ApiResp a)
readCdn77Response reqAff = ExceptT $ attempt reqAff >>= \respAttempt → pure $ do
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
              (stringify <$> lookup "description" obj)
            let errorDetails = case stringify <$> lookup "errors" obj of
                  Nothing -> ""
                  Just errs -> " Errors: " <> errs
            Left $ ResourceError (desc <> errorDetails)
          else readJson body)
    body
  where
    readStatusError = ServerReponseError "Can't read response status"

    withError err = case _ of
      Nothing → Left err
      Just a  → Right a

    -- [XXX][WARNING] be careful when using out of scope
    -- this function will downcase keys in nested js objects! which may results in parser errors
    readJson ∷ ∀ a'. ReadForeign a' ⇒ Json → Either ApiCallError a'
    readJson = errorType (ReturnedObjectTypeError <<< show ) <<< read <<< coerceJson
      where
        coerceJson ∷ Json → Foreign
        coerceJson = coerceJsonHelperImpl <<< unsafeCoerce

    -- [XXX][WARNING] be careful when using out of scope
    -- this function will downcase keys in nested js objects! which may results in parser errors
    writeJson ∷ ∀ a'. WriteForeign a' ⇒ a' → Json
    writeJson = coerceJson <<< write
      where
        coerceJson ∷ Foreign → Json
        coerceJson = unsafeCoerce
