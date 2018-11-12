module Cdn77
 ( createCdnResource
 , listCdnResources
 , getCdnResourceDetails
 , prefetch
 , purge
 , purgeAll
 , listRequests
 , getRequestDetails
 , listRequestUrl
 , createStorage
 , storageDetails
 , deleteStorage
 , listStorages
 , addStorageCdnResources
 , listStorageLocations
 , reportDetails
 ) where

import Affjax (Response)
import Affjax (get, post) as AffJax
import Affjax.RequestBody (formURLEncoded) as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Common (readNonStandardResponse, readResponsesCustomObject, readResponsesOptionalCustomObject, readStandardResponse)
import Control.Applicative (void)
import Control.Category ((<<<))
import Control.Monad.Except (ExceptT)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.FormURLEncoded (encode)
import Data.Function (const, ($))
import Data.Functor (map)
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Debug.Trace (trace)
import Effect.Aff (Aff)
import Simple.JSON (class WriteForeign)
import Types (ApiCallError, ApiRequest, ApiRequestUrl, ApiResponse, CDNResourceDetails, CdnId, Report, ReportType, RequestId, RequestType, ResourceType, Storage, StorageId, StorageLocation, StorageLocationId, Timestamp, splitProtocols)
import Utils (urlEncoded)


apiUrl ∷ String
apiUrl = "https://api.cdn77.com/v2.0"

get ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
get endpoint params = AffJax.get ResponseFormat.json $
  let
    u = apiUrl <> endpoint <> "?" <> encode (urlEncoded params)
  in trace u $ const u

post ∷ ∀ p. WriteForeign { | p } ⇒ String → { | p } → Aff (Response (Either ResponseFormat.ResponseFormatError Json))
post endpoint params = 
  trace uri $ const $ trace body $ const $ AffJax.post ResponseFormat.json uri body
  where
    uri = (apiUrl <> endpoint)
    body = (RequestBody.formURLEncoded $ urlEncoded params)



-------------------------------------------------------
--------------------- CDNResources --------------------

createCdnResource
  ∷ { login ∷ String, passwd ∷ String, label ∷ String, type ∷ ResourceType }
  → ExceptT ApiCallError Aff CDNResourceDetails
createCdnResource = readResponsesCustomObject "cdnResource" <<< post "/cdn-resource/create"

listCdnResources
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (Array CDNResourceDetails)
listCdnResources = readResponsesOptionalCustomObject "cdnResources" [] <<< get "/cdn-resource/list"

getCdnResourceDetails
  ∷ { id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff CDNResourceDetails
getCdnResourceDetails = readResponsesCustomObject "cdnResource" <<< get "/cdn-resource/details"

-------------------------------------------------------
------------------------ DATA -------------------------

prefetch
  ∷ { login ∷ String, passwd ∷ String, cdn_id ∷ CdnId, url ∷ Array String }
  → ExceptT ApiCallError Aff {url :: Array String, request_id :: RequestId}
prefetch = map unwrapResp <<< readNonStandardResponse <<< post "/data/prefetch"

purge
  ∷ { url ∷ Array String, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff {url ∷ Array String, request_id ∷ RequestId }
purge = map unwrapResp <<< readNonStandardResponse <<< post "/data/purge"

purgeAll
  ∷ { cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff Unit
purgeAll = void <<< readStandardResponse <<< post "/data/purge-all"

unwrapResp
  :: ApiResponse (url ∷ Array String, request_id ∷ RequestId)
  -> {url :: Array String, request_id :: RequestId}
unwrapResp {url, request_id} = {url, request_id}

--------------------------------------------------------
---------------------- DATA QUEUE ----------------------

-- | Lists requests made to Cdn with cdn_id id. Returns error if none.
listRequests
  ∷ { type ∷ RequestType, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (Array ApiRequest)
listRequests = readResponsesOptionalCustomObject "requests" [] <<< get "/data-queue/list-request"

getRequestDetails
  ∷ { id ∷ RequestId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff ApiRequest
getRequestDetails = readResponsesCustomObject "request" <<< get "/data-queue/details-request"

listRequestUrl
  ∷ { request_id ∷ RequestId, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff ApiRequestUrl
listRequestUrl = readResponsesCustomObject "urls" <<< get "/data-queue/list-url"


--------------------------------------------------------
------------------------ STORAGE -----------------------

createStorage
  ∷ { login ∷ String, passwd ∷ String, zone_name ∷ String, storage_location_id ∷ StorageLocationId }
  → ExceptT ApiCallError Aff Storage
createStorage = map splitProtocols <<< readResponsesCustomObject "storage" <<< post "/storage/create"

storageDetails
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId }
  → ExceptT ApiCallError Aff Storage
storageDetails = map splitProtocols <<< readResponsesCustomObject "storage" <<< get "/storage/details"

deleteStorage
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId }
  → ExceptT ApiCallError Aff Unit
deleteStorage = void <<< readStandardResponse <<< post "/storage/delete"

listStorages
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (Array Storage)
listStorages = map (map splitProtocols) <<< readResponsesOptionalCustomObject "storages" [] <<< get "/storage/list"

addStorageCdnResources
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId, cdn_ids ∷ Array CdnId }
  → ExceptT ApiCallError Aff Unit
addStorageCdnResources = void <<< readStandardResponse <<< post "/storage/add-cdn-resource"


---------------------------------------------------------
------------------ STORAGE LOCATION ---------------------

listStorageLocations
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (Array StorageLocation)
listStorageLocations = readResponsesOptionalCustomObject "storageLocations" [] <<< get "/storage-location/list"


--------------------------------------------------------
------------------------ REPORT -----------------------

reportDetails
  ∷ { login ∷ String, passwd ∷ String, type ∷ ReportType
    , from ∷ Timestamp, to ∷ Timestamp, cdn_ids ∷ Array CdnId}
  → ExceptT ApiCallError Aff Report
reportDetails = readResponsesCustomObject "report" <<< get "/report/details"
