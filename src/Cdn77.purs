module Cdn77 where

import Affjax (Response)
import Affjax (get, post) as AffJax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Category ((<<<))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function (const, ($))
import Data.Semigroup ((<>))
import Data.Unit (Unit, unit)
import Effect.Aff (Aff)
import Simple.JSON (class WriteForeign)
import Types (ApiCallError, ApiRequest, ApiRequestUrl, CDNResource, CDNResourceDetails, CdnId, RequestId, RequestType)
import Utils (jsonGetParams, readJson, readResponsesCustomObject, readStandardResponse, writeJson)

apiUrl ∷ String
apiUrl = "https://api.cdn77.com/v2.0"


get ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
get endpoint params = AffJax.get ResponseFormat.json $ endpoint <> "?" <> jsonGetParams params

post ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
post endpoint params = AffJax.post ResponseFormat.json endpoint (RequestBody.json $ writeJson params)

-------------------------------------------------------
--------------------- CDNResources --------------------

listCdnResources ∷ { login∷ String, passwd ∷ String} → Aff (Either ApiCallError (Array CDNResource))
listCdnResources params = do
  cdnResources ← readResponsesCustomObject "cdnResources" (get "/cdn-resource/list" params)
  pure (readJson =<< cdnResources)

getCdnResourceDetails ∷ { id ∷ CdnId, login∷ String, passwd ∷ String} → Aff (Either ApiCallError CDNResourceDetails)
getCdnResourceDetails params = do
  cdnResource ← readResponsesCustomObject "cdnResource"
    (get "/cdn-resource/details?" params)
  pure (readJson =<< cdnResource)

-------------------------------------------------------
------------------------ DATA -------------------------

prefetch
  ∷ { url ∷ Array String, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String}
  → Aff (Either ApiCallError {url ∷ Array String, request_id ∷ RequestId })
prefetch params = do
  cdnResource ← readResponsesCustomObject "cdnResource" (post "/data/prefetch" params)
  pure $ readJson =<< cdnResource

purge
  ∷ { url ∷ Array String, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String}
  → Aff (Either ApiCallError {url ∷ Array String, request_id ∷ RequestId })
purge params = do
  cdnResource ← readResponsesCustomObject "cdnResource" (post "/data/purge" params)
  pure $ readJson =<< cdnResource

purgeAll
  ∷ { url ∷ Array String, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String}
  → Aff (Either ApiCallError Unit)
purgeAll params = pure <<< const (Right unit) =<< readStandardResponse (post "/data/purge" params)

--------------------------------------------------------
---------------------- DATA QUEUE ----------------------

listRequests
  ∷ { type ∷ RequestType, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String}
  → Aff (Either ApiCallError (Array ApiRequest))
listRequests params = do
  requests ← readResponsesCustomObject "requests"
    (get "/data-queue/list-request?" params)
  pure (readJson =<< requests)

getRequestDetails
  ∷ { id ∷ RequestId, login ∷ String, passwd ∷ String}
  → Aff (Either ApiCallError ApiRequest)
getRequestDetails params = do
  requests ← readResponsesCustomObject "request"
    (get "/data-queue/details-request?" params)
  pure (readJson =<< requests)

listRequestUrl
  ∷ { request_id ∷ RequestId, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String}
  → Aff (Either ApiCallError ApiRequestUrl)
listRequestUrl params = do
  requests ← readResponsesCustomObject "urls"
    (get "/data-queue/details-request?" params)
  pure (readJson =<< requests)
