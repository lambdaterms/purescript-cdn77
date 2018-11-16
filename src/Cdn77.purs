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
import Default (class NothingFields, Justify(..), Nothingify, buildDefault)
import Effect.Aff (Aff)
import Heterogeneous.Mapping (class HMap, hmap)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Request (get, post, post2)
import Response (readCdn77Response)
import Simple.JSON (class WriteForeignFields)
import Type.Row (RProxy(..))
import Types (ApiCallError, ApiRequest, ApiRequestUrl, ApiResp, CDNResourceDetails, Cdn77CreateResourceConfig, ResourceId, Report, ReportType, RequestId, RequestType, ResourceType, Storage, StorageId, StorageLocation, StorageLocationId, Timestamp, Cdn77EditResourceConfig, splitProtocols)

-------------------------------------------------------
--------------------- CDNResources --------------------

createCdnResource
  ∷ ∀ provided providedJustified missing  allNothingified ret retNubbed nrl rl x
  -- | Scaffolding for automatic filling of not-provided optional arguments
  . HMap Justify { | provided } { | providedJustified }
  ⇒ HMap Nothingify { | Cdn77CreateResourceConfig } { | allNothingified }
  ⇒ Union providedJustified allNothingified ret
  ⇒ RowToList allNothingified nrl
  ⇒ NothingFields nrl () allNothingified
  ⇒ Nub ret retNubbed
  ⇒ RowToList retNubbed rl
  ⇒ WriteForeignFields rl retNubbed () x
-- | Focus on this type when using the function
  ⇒ Union provided missing Cdn77CreateResourceConfig
  ⇒ { | provided}
  → { login ∷ String, passwd ∷ String, storage_id ∷ StorageId, label ∷ String, type ∷ ResourceType }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
createCdnResource opts =
  let
    allOpts = merge (hmap Justify opts) (buildDefault (RProxy :: RProxy Cdn77CreateResourceConfig))
  in
    readCdn77Response <<< post2 "/cdn-resource/create" allOpts

createCdnResource_
  ∷ { login ∷ String, passwd ∷ String, storage_id ∷ StorageId, label ∷ String, type ∷ ResourceType }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
createCdnResource_ = readCdn77Response <<< post "/cdn-resource/create"

editCdnResource
  ∷ ∀ provided providedJustified missing  allNothingified ret retNubbed nrl rl x
  -- | Scaffolding for automatic filling of not-provided optional arguments
  . HMap Justify { | provided } { | providedJustified }
  ⇒ HMap Nothingify { | Cdn77EditResourceConfig } { | allNothingified }
  ⇒ Union providedJustified allNothingified ret
  ⇒ RowToList allNothingified nrl
  ⇒ NothingFields nrl () allNothingified
  ⇒ Nub ret retNubbed
  ⇒ RowToList retNubbed rl
  ⇒ WriteForeignFields rl retNubbed () x
-- | Focus on this type when using the function
  ⇒ Union provided missing Cdn77EditResourceConfig
  ⇒ { | provided}
  → { login ∷ String, passwd ∷ String, id ∷ ResourceId }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
editCdnResource opts =
  let
    allOpts = merge (hmap Justify opts) (buildDefault (RProxy :: RProxy Cdn77EditResourceConfig))
  in
    readCdn77Response <<< post2 "/cdn-resource/edit" allOpts

editCdnResource_
  ∷ { login ∷ String, passwd ∷ String, id ∷ ResourceId }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
editCdnResource_ = readCdn77Response <<< post "/cdn-resource/edit"

listCdnResources
  ∷ { login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (cdnResources ∷ Array CDNResourceDetails))
listCdnResources = readCdn77Response <<< get "/cdn-resource/list"

getCdnResourceDetails
  ∷ { id ∷ ResourceId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (cdnResource ∷ CDNResourceDetails))
getCdnResourceDetails = readCdn77Response <<< get "/cdn-resource/details"


-------------------------------------------------------
------------------------ DATA -------------------------

prefetch
  ∷ { login ∷ String, passwd ∷ String, cdn_id ∷ ResourceId, url ∷ Array String }
  → ExceptT ApiCallError Aff (ApiResp (url :: Array String, request_id :: RequestId))
prefetch = readCdn77Response <<< post "/data/prefetch"

purge
  ∷ { url ∷ Array String, cdn_id ∷ ResourceId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (url :: Array String, request_id :: RequestId))
purge = readCdn77Response <<< post "/data/purge"

purgeAll
  ∷ { cdn_id ∷ ResourceId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp ())
purgeAll = readCdn77Response <<< post "/data/purge-all"


--------------------------------------------------------
---------------------- DATA QUEUE ----------------------

-- | Lists requests made to Cdn with cdn_id id. Returns error if none.
listRequests
  ∷ { type ∷ RequestType, cdn_id ∷ ResourceId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (requests ∷ Array ApiRequest))
listRequests = readCdn77Response <<< get "/data-queue/list-request"

getRequestDetails
  ∷ { id ∷ RequestId, login ∷ String, passwd ∷ String }
  → ExceptT ApiCallError Aff (ApiResp (request ∷ ApiRequest))
getRequestDetails = readCdn77Response <<< get "/data-queue/details-request"

listRequestUrl
  ∷ { request_id ∷ RequestId, cdn_id ∷ ResourceId, login ∷ String, passwd ∷ String }
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
  ∷ { login ∷ String, passwd ∷ String, id ∷ StorageId, cdn_ids ∷ Array ResourceId }
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
    , from ∷ Timestamp, to ∷ Timestamp, cdn_ids ∷ Array ResourceId}
  → ExceptT ApiCallError Aff (ApiResp (report ∷ Report))
reportDetails = readCdn77Response <<< get "/report/details"


--------------------------------------------------------
---------------------- AUXILLIARY -----------------------

splitStorageProtocols
  ∷ ∀ t4 t5 t6
  . { storage ∷ { credentials ∷ { protocol ∷ String | t5} | t4} | t6}
  → { storage ∷ { credentials ∷ { protocol ∷ Array String | t5} | t4} | t6}
splitStorageProtocols r = r{ storage = splitProtocols r.storage }
