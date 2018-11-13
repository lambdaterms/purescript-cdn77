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

import Control.Category ((<<<))
import Control.Monad.Except (ExceptT)
import Data.Functor (map)
import Effect.Aff (Aff)
import Request (get, post)
import Response (readCdn77Response)
import Types (ApiCallError, ApiRequest, ApiRequestUrl, ApiResp, CDNResourceDetails, CdnId, Report, ReportType, RequestId, RequestType, ResourceType, Storage, StorageId, StorageLocation, StorageLocationId, Timestamp, splitProtocols)

-------------------------------------------------------
--------------------- CDNResources --------------------

createCdnResource
  ∷ { login ∷ String, passwd ∷ String, storage_id ∷ StorageId, label ∷ String, type ∷ ResourceType }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
createCdnResource = readCdn77Response <<< post "/cdn-resource/create"

listCdnResources
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (cdnResources ∷ Array CDNResourceDetails))
listCdnResources = readCdn77Response <<< get "/cdn-resource/list"

getCdnResourceDetails
  ∷ { id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
getCdnResourceDetails = readCdn77Response <<< get "/cdn-resource/details"


-------------------------------------------------------
------------------------ DATA -------------------------

prefetch
  ∷ { login ∷ String, passwd ∷ String, cdn_id ∷ CdnId, url ∷ Array String }
  → ExceptT ApiCallError Aff (ApiResp (url :: Array String, request_id :: RequestId))
prefetch = readCdn77Response <<< post "/data/prefetch"

purge
  ∷ { url ∷ Array String, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (url :: Array String, request_id :: RequestId))
purge = readCdn77Response <<< post "/data/purge"

purgeAll
  ∷ { cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp ())
purgeAll = readCdn77Response <<< post "/data/purge-all"


--------------------------------------------------------
---------------------- DATA QUEUE ----------------------

-- | Lists requests made to Cdn with cdn_id id. Returns error if none.
listRequests
  ∷ { type ∷ RequestType, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (requests ∷ Array ApiRequest))
listRequests = readCdn77Response <<< get "/data-queue/list-request"

getRequestDetails
  ∷ { id ∷ RequestId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (request ∷ ApiRequest))
getRequestDetails = readCdn77Response <<< get "/data-queue/details-request"

listRequestUrl
  ∷ { request_id ∷ RequestId, cdn_id ∷ CdnId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (urls ∷ ApiRequestUrl))
listRequestUrl = readCdn77Response <<< get "/data-queue/list-url"


--------------------------------------------------------
------------------------ STORAGE -----------------------

createStorage
  ∷ { login ∷ String, passwd ∷ String, zone_name ∷ String, storage_location_id ∷ StorageLocationId }
  → ExceptT ApiCallError Aff (ApiResp (storage ∷ Storage))
createStorage = map splitStorageProtocols <<< readCdn77Response <<< post "/storage/create"

storageDetails
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId }
  → ExceptT ApiCallError Aff (ApiResp (storage ∷ Storage))
storageDetails = map splitStorageProtocols <<< readCdn77Response <<< get "/storage/details"

deleteStorage
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId }
  → ExceptT ApiCallError Aff (ApiResp ())
deleteStorage = readCdn77Response <<< post "/storage/delete"

listStorages
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (storages ∷ Array Storage))
listStorages = map split <<< readCdn77Response <<< get "/storage/list"
  where
    split r = r{ storages = map splitProtocols r.storages }

addStorageCdnResources
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId, cdn_ids ∷ Array CdnId }
  → ExceptT ApiCallError Aff (ApiResp ())
addStorageCdnResources = readCdn77Response <<< post "/storage/add-cdn-resource"


---------------------------------------------------------
------------------ STORAGE LOCATION ---------------------

listStorageLocations
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (storageLocations ∷ Array StorageLocation))
listStorageLocations = readCdn77Response <<< get "/storage-location/list"


--------------------------------------------------------
------------------------ REPORT -----------------------

reportDetails
  ∷ { login ∷ String, passwd ∷ String, type ∷ ReportType
    , from ∷ Timestamp, to ∷ Timestamp, cdn_ids ∷ Array CdnId}
  → ExceptT ApiCallError Aff (ApiResp (report ∷ Report))
reportDetails = readCdn77Response <<< get "/report/details"


--------------------------------------------------------
---------------------- AUXILLIARY -----------------------

splitStorageProtocols
  ∷ ∀ t4 t5 t6
  . { storage ∷ { credentials ∷ { protocol ∷ String | t5} | t4} | t6}
  → { storage ∷ { credentials ∷ { protocol ∷ Array String | t5} | t4} | t6}
splitStorageProtocols r = r{ storage = splitProtocols r.storage }
