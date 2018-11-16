module Types where

import Control.Applicative (pure)
import Control.Bind (bind, (>=>), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except (except, runExceptT)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String (Pattern(..), split, trim)
import Data.Traversable (sequence)
import Foreign (F, Foreign, ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl, writeJSON)
import Utils (coerceJsonHelperImpl)


type ApiResponse a =
  { description ∷ String
  , result ∷ a }

type NonDefaultApiResponse dataR =
  { status ∷ String
  , description ∷ String
  | dataR
  }

type ApiResp dataR =
  { status ∷ String
  , description ∷ String
  | dataR
  }


-- [TODO]
type Timestamp = Int

data ApiCallError = RequestError String
                  | ServerReponseError String
                  | ReturnedObjectTypeError String
                  | ResourceError String

instance showApiCallError ∷ Show ApiCallError where
  show = case _ of
    RequestError x → "RequestError " <> x
    ServerReponseError x → "ServerReponseError " <> x
    ReturnedObjectTypeError x → "ReturnedObjectTypeError " <> x
    ResourceError x → "ResourceError " <> x

readImplStringOrInt ∷ Foreign → F String
readImplStringOrInt frn = do
    case unwrap $ runExceptT (readInt frn) of
      Right i → pure (show i)
      Left x → readImpl frn
    where
      readInt ∷ Foreign → F Int
      readInt = readImpl

newtype ResourceId = ResourceId String

instance readForeignResourceId ∷ ReadForeign ResourceId where
  readImpl = map ResourceId <<< readImplStringOrInt
derive newtype instance writeForeignResourceId ∷ WriteForeign ResourceId
derive newtype instance showResourceId ∷ Show ResourceId
derive newtype instance eqResourceId ∷ Eq ResourceId


newtype UrlId = UrlId String

instance readForeignUrlId ∷ ReadForeign UrlId where
  readImpl = map UrlId <<< readImplStringOrInt
derive newtype instance writeForeignUrlId ∷ WriteForeign UrlId
derive newtype instance showUrlId ∷ Show UrlId
derive newtype instance eqUrlId ∷ Eq UrlId

newtype RequestId = RequestId String

instance readForeignRequestId ∷ ReadForeign RequestId where
  readImpl = map RequestId <<< readImplStringOrInt
derive newtype instance writeForeignRequestId ∷ WriteForeign RequestId
derive newtype instance showRequestId ∷ Show RequestId
derive newtype instance eqRequestId ∷ Eq RequestId

newtype StorageId = StorageId String

instance readForeignStorageId ∷ ReadForeign StorageId where
  readImpl = map StorageId <<< readImplStringOrInt
derive newtype instance writeForeignStorageId ∷ WriteForeign StorageId
derive newtype instance showStorageId ∷ Show StorageId
derive newtype instance eqStorageId ∷ Eq StorageId

newtype StorageLocationId = StorageLocationId String

instance readForeignStorageLocationId ∷ ReadForeign StorageLocationId where
  readImpl = map StorageLocationId <<< readImplStringOrInt
derive newtype instance writeForeignStorageLocationId ∷ WriteForeign StorageLocationId
derive newtype instance showStorageLocationId ∷ Show StorageLocationId
derive newtype instance eqStorageLocationId ∷ Eq StorageLocationId

data Switch = On | Off

derive instance eqSwitch ∷ Eq Switch

fromBool ∷ Boolean → Switch
fromBool = case _ of
  true → On
  false → Off

asBool ∷ Switch → Boolean
asBool = case _ of
  On → true
  Off → false

instance writeForeignSwitch ∷ WriteForeign Switch where
  writeImpl = case _ of
    On → writeImpl 1
    Off → writeImpl 0

instance readForeignSwitch ∷ ReadForeign Switch where
  readImpl frn = readImpl frn >>= case _ of
    0 → pure On
    1 → pure Off
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match switch value. Should be '0' or '1'. Was: '" <> show x <> "'")

data RequestType = Prefetch | Purge

derive instance eqRequestType ∷ Eq RequestType

instance writeForeignRequestType ∷ WriteForeign RequestType where
  writeImpl = case _ of
    Prefetch → writeImpl "prefetch"
    Purge → writeImpl "purge"

instance readForeignRequestType ∷ ReadForeign RequestType where
  readImpl frn = readImpl frn >>= case _ of
    "prefetch" → pure Prefetch
    "purge" → pure Purge
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match request type value. Should be 'purge' or 'prefetch'. Was: '" <> x <> "'")


data FilterType = Whitelist | Blacklist

derive instance eqFilterType ∷ Eq FilterType

instance writeForeignFilterType ∷ WriteForeign FilterType where
  writeImpl = case _ of
    Blacklist → writeImpl "blacklist"
    Whitelist → writeImpl "whitelist"

instance readForeignFilterType ∷ ReadForeign FilterType where
  readImpl frn = readImpl frn >>= case _ of
    "blacklist" → pure Blacklist
    "whitelist" → pure Whitelist
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match request type value. Should be 'whitelist' or 'blacklist'. Was: '" <> x <> "'")

data OriginScheme = HttpScheme | HttpsScheme

derive instance eqOriginScheme ∷ Eq OriginScheme

instance writeForeignOriginScheme ∷ WriteForeign OriginScheme where
  writeImpl = case _ of
    HttpScheme → writeImpl "http"
    HttpsScheme → writeImpl "https"

instance readForeignOriginScheme ∷ ReadForeign OriginScheme where
  readImpl frn = readImpl frn >>= case _ of
    "http" → pure HttpScheme
    "https" → pure HttpsScheme
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match request type value. Should be 'http' or 'https'. Was: '" <> x <> "'")


data UrlSigningType = UrlSigningPath | UrlSigningParameter
derive instance eqUrlSigningType ∷ Eq UrlSigningType

instance writeForeignUrlSigningType ∷ WriteForeign UrlSigningType where
  writeImpl = case _ of
    UrlSigningPath → writeImpl "path"
    UrlSigningParameter → writeImpl "parameter"

instance readForeignUrlSigningType ∷ ReadForeign UrlSigningType where
  readImpl frn = readImpl frn >>= case _ of
    "path" → pure UrlSigningPath
    "paramete" → pure UrlSigningParameter
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match url signing type value. Should be 'path' or 'parameter'. Was: '" <> x <> "'")

data ResourceType = ResourceTypeStandard | ResourceTypeVideo
derive instance eqResourceType ∷ Eq ResourceType

instance writeForeignResourceType ∷ WriteForeign ResourceType where
  writeImpl = case _ of
    ResourceTypeStandard → writeImpl "standard"
    ResourceTypeVideo → writeImpl "video"

instance readForeignResourceType ∷ ReadForeign ResourceType where
  readImpl frn = readImpl frn >>= case _ of
    "standard" → pure ResourceTypeStandard
    "video" → pure ResourceTypeVideo
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match resource type value. Should be 'standard' or 'video'. Was: '" <> x <> "'")

------------------------------------------
disableStorage ∷ StorageId
disableStorage = StorageId "0"

type Cdn77CreateResourceConfig =
  ( origin_scheme ∷ OriginScheme  -- URL scheme of the Origin. Valid values: 'http' | 'https'
  , origin_url ∷ String  -- www.domain.ltd 	URL of your content source (Origin). Doesn't have to be set when CDN Storage Id is set (that means instead of using your own URL you use our CDN Storage).
  , origin_port ∷ Int -- You can specify port through which we will access your origin.
  , storage_id ∷ StorageId  -- Storage Id. See available Storages and their Id in the list of storages. Set to 0 if you want to disable CDN Storage. Ignore query string (qs_status) is set to 1 when you enable CDN Storage as Origin.
  , cname ∷ String -- Domain name that will redirect to our CDN server.
  , other_cnames ∷ Array String  -- Array. Maximum length of array is 9.
  , cache_expiry ∷ Int	-- In minutes. Valid values: '10' | '10800' | '11520' | '12960' | '1440' | '14440' | '15840' | '17280' | '2160' | '240' | '2880' | '30' | '4320' | '5760' | '60' | '720' | '7200' | '8640'
  , qs_status ∷ Switch  -- By default the entire URL is treated as a separate cacheable item. If you want to override this, set qs_status to '1', otherwise to '0'. If you have CDN Storage set as Origin, qs_status is automatically set to 1. Valid values: '0' | '1'
  , setcookie_status ∷ Switch  -- To cache Set-Cookies responses, set this to '1' (disabled by default). Valid values: '0' | '1'
  , mp4_pseudo_on ∷ Switch -- Allow streaming video in mp4 format. For video, a CDN Resource is automatically enabled. Valid values: '0' | '1'
  , gp_type ∷ FilterType  -- Sets geo protection type. Valid values: 'blacklist' | 'whitelist'
  , gp_countries ∷ Array String -- ["FR", "GB", "US"]. Sets geo protection list of whitelisted/blacklisted countries, enter the country's 2 character ISO code.
  , ipp_type ∷ FilterType  -- Sets IP protection type. Valid values: 'blacklist' | 'whitelist'
  , ipp_addresses ∷ Array String  -- ["192.168.25.0/24", “72.53.0.0/16"]. Sets IP protection list of whitelisted/blacklisted addresses. Accepts CIDR notation only.
  , url_signing_on_∷ Switch  -- Allow generating of secured links with expiration time. Content is not available without valid token. Valid values: '0' | '1'
  , url_signing_type ∷ UrlSigningType  --Sets secure token type Valid values: 'parameter' | 'path'
  , url_signing_key ∷ String  -- Key (hash) for signing URLs.
)


type Cdn77EditResourceConfig =
  ( label ∷ String  -- Your own label of a CDN Resource.
  , location_group ∷ Int  -- Location group of CDN resource. Users cannot change it.
  , hlp_type ∷ FilterType -- Sets hotlink protection type Valid values: 'blacklist' | 'whitelist'
  , hlp_referer_domains ∷ Array String  -- Sets hotlink protection list of whitelisted/blacklisted referer domains
  , hlp_deny_empty_referer ∷ Switch  -- Sets hotlink protection denying access with empty referer Valid values: '0' | '1
  , instant_ssl ∷ Switch  -- Set to 1 if you want to have a SSL certificate for every CNAME for free.
  | Cdn77CreateResourceConfig
  )

type CDNResourceBase =
  ( id ∷ ResourceId  -- Your CDN Id. See how to retrieve a list of your cdns including their ids.
  , label ∷ String -- Your own label of a CDN Resource.
  , origin_url ∷ String -- URL of your content source (Origin). Doesn't have to be set when CDN Storage Id is set (that means instead of using your own URL you use our CDN Storage).
  , cname ∷ String -- Domain name that will redirect to our CDN server.
  , cache_expiry ∷ Int -- In minutes. Valid values: '10' | '10800' | '11520' | '12960' | '1440' | '14440' | '15840' | '17280' | '2160' | '240' | '2880' | '30' | '4320' | '5760' | '60' | '720' | '7200' | '8640'
  , url_signing_on ∷ Switch -- Allow generating of secured links with expiration time. Content is not available without valid token. Valid values: '0' | '1'
  , url_signing_key ∷ Nullable String -- Key (hash) for signing URLs.
  , instant_ssl ∷ Switch -- Set to 1 if you want to have a SSL certificate for every CNAME for free.
  , type ∷ Maybe (Nullable ResourceType) -- Valid values: 'standard' | 'video'
  , storage_id ∷ Nullable StorageId -- Storage Id. See available Storages and their Id in the list of storages. Set to 0 if you want to disable CDN Storage. Ignore query string (qs_status) is set to 1 when you enable CDN Storage as Origin.
  , qs_status ∷ Switch -- By default the entire URL is treated as a separate cacheable item. If you want to override this, set qs_status to '1', otherwise to '0'. If you have CDN Storage set as Origin, qs_status is automatically set to 1. Valid values: '0' | '1'
  , setcookie_status ∷ Switch -- To cache Set-Cookies responses, set this to '1' (disabled by default). Valid values: '0' | '1'
  , other_cnames ∷ Array String  -- Array. Maximum length of array is 9.
  , mp4_pseudo_on ∷ Switch -- Allow streaming video in mp4 format. For video, a CDN Resource is automatically enabled. Valid values: '0' | '1'
  )

type CDNResourceAdditional base =
  ( gp_countries ∷ Array String -- Sets geo protection list of whitelisted/blacklisted countries, enter the country's 2 character ISO code.
  , gp_type ∷ Nullable FilterType -- Sets geo protection type. Valid values: 'blacklist' | 'whitelist'
  , ipp_addresses ∷ Array String -- Sets IP protection list of whitelisted/blacklisted addresses. Accepts CIDR notation only.
  , ipp_type ∷ Nullable FilterType -- Sets IP protection type. Valid values: 'blacklist' | 'whitelist'
  , platform ∷ String -- Check more about our new NeXt Generation platform. Valid values: 'nxg' | 'old'
  , cdn_url ∷ String -- ?
  , origin_port ∷ Maybe (Nullable Int)  -- You can specify port through which we will access your origin.
  , origin_scheme ∷ OriginScheme  -- URL scheme of the Origin. Valid values: 'http' | 'https'
  , https_redirect_code ∷ Maybe (Nullable String) -- not documented
  , ignored_query_params ∷ Maybe (Array String) -- not documented
  , hlp_type ∷ Maybe (Nullable FilterType) -- not documented
  , hlp_deny_empty_referer ∷ Maybe (Nullable Switch) -- not documented
  , hlp_referer_domains ∷ Maybe (Array String) -- Sets hotlink protection list of whitelisted/blacklisted referer domains
  , http2 ∷ Maybe (Nullable Switch) -- not documented
  , streaming_playlist_bypass ∷ Maybe (Nullable Switch) -- not documented
  , forward_host_header ∷ Maybe (Nullable Switch)-- not documented
  , url_signing_type ∷ Maybe (Nullable String) -- not documented
  | base
  )
type CDNResource = Record CDNResourceBase

type CDNResourceDetails = Record (CDNResourceAdditional CDNResourceBase)


type ApiRequest =
  { id ∷ RequestId -- Queued request ID. You will retrieve this ID in your purge/prefetch request.
  , cdn_id ∷ ResourceId -- CDN Id. See how to retrieve a list of your CDNs including their Ids.
  , type ∷ RequestType -- Type of request. Valid values: 'prefetch' | 'purge'
  , created ∷ Timestamp  -- Timestamp of creation.
  , finished ∷ Nullable Timestamp -- Your request receives a timestamp when it has been finished (successfully or not).
  , url_successful ∷ Int -- Amount of successfully proceeded URL. Until the request is finished, this number may increase in time.
  , url_total ∷ Int --	Total amount of URL in a request.
  , waiting_for_request_id ∷ Nullable Int -- When enabling the 'purge_first' option on the prefetch method, the urls are saved into the queue twice - first into the purge and then to the prefetch. To ensure the purge is proceeded first, we add the id of the purge request into the prefetch as value waiting_for_request_id. Valid values: null or int.
  }

type ApiRequestUrl =
 { id ∷ UrlId
 , request_id ∷ RequestId
 , url ∷ String
 , finished ∷ Nullable Timestamp -- Until the URL has proceeded value of 'finished’ is NULL. After that the timestamp is shown. Remember that an URL may be marked as finished whether it was proceeded successfully or not.
 , finished_successfully ∷ Boolean -- Whether the purge or prefetch of a given URL was successful or not.
 }


--------------------- STORAGE ---------------------

type StorageCredentials =
  { protocol ∷ Array String
  , host ∷ String
  , user ∷ String
  , pass ∷ String }

type Storage =
 { id ∷ StorageId -- Retrieve list of storages including their ids with the list method.
 , zone_name ∷ String -- Zone name has to be unique. Allowed characters: latin alphabet characters, numbers, spaces and '-', '_' symbols.
 , storage_location_id ∷ StorageLocationId -- Storage Location Id. Retrieve list of available storage locations including their ids with the list storage location method.
 , used_space ∷ String -- TODO this would be nicer with some proper parsing. Amount of space used by your data on the CDN Storage.
 , cdn_resources ∷ Array ResourceId -- IDs of CDN Resources using this storage.
 , credentials ∷ StorageCredentials -- Object with params: protocol, host, user, pass.
 }


splitProtocols
  ∷ ∀ r r1
  . { credentials ∷ { protocol ∷ String | r} | r1}
  → { credentials ∷ { protocol ∷ Array String | r} | r1}
splitProtocols s =
  let prot = trim <$> split (Pattern ",") s.credentials.protocol
  in s{credentials{protocol=prot}}

---------------- STORAGE LOCATION ---------------------

type StorageLocation =
  { id ∷ StorageLocationId
  , name ∷ String }

--------------------- REPORT ---------------------

data ReportType
  = Bandwidth
  | Costs
  | HitMiss
  | Traffic

derive instance eqReportType ∷ Eq ReportType

instance writeForeignReportType ∷ WriteForeign ReportType where
  writeImpl = case _ of
    Bandwidth → writeImpl "bandwidth"
    Costs → writeImpl "costs"
    HitMiss → writeImpl "hit-miss"
    Traffic → writeImpl "traffic"

instance readForeignReportType ∷ ReadForeign ReportType where
  readImpl frn = readImpl frn >>= case _ of
    "bandwidth" → pure Bandwidth
    "costs" → pure Costs
    "hit-miss" → pure HitMiss
    "traffic" → pure Traffic
    x → except $ Left (singleton $ ForeignError $ "Couldn't match RequestType value. Should be one of:     'bandwidth', 'costs' , 'hit-miss', 'traffic'. Was: '" <> show x <> "'")

data ReportUnit
  = Usd -- for costs
  | Bytes -- for traffic
  | Bps -- for bandwidth

instance writeForeignReportUnit ∷ WriteForeign ReportUnit where
  writeImpl = case _ of
    Usd → writeImpl "USD"
    Bytes → writeImpl "B"
    Bps → writeImpl "bps"

instance showReportUnit ∷ Show ReportUnit where
  show = writeJSON

instance readForeignReportUnit ∷ ReadForeign ReportUnit where
  readImpl frn = readImpl frn >>= case _ of
    "USD" → pure Usd
    "B" → pure Bytes
    "bps" → pure Bps
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match ReportUnit value. Should be 'USD', 'B' or 'bps'. Was: '" <> show x <> "'")

derive instance eqReportUnit ∷ Eq ReportUnit

type RegionsReport =
  { eu ∷ Number
  , sa ∷ Number
  , na ∷ Number
  , as ∷ Number
  , au ∷ Number
  , af ∷ Number }

newtype ReportData = ReportData (Array { cdnId ∷ ResourceId , regions ∷ RegionsReport } )

derive instance eqReportData ∷ Eq ReportData

instance showReportData ∷ Show ReportData where
  show (ReportData rd)= show rd

foreign import parseReportStructureImpl
  ∷ ∀ a
  . (String → F ReportData) -- error function
  → (a → F a) -- success function
  → Foreign
  → F (Array {cdnId ∷ Foreign, regions ∷ Foreign})


instance readForeignReportData ∷ ReadForeign ReportData where
  readImpl = (parseReportStructureImpl err pure <<< coerceJsonHelperImpl) >=> (map ReportData <<< sequence <<< map parseRegions)

    where
      err msg = except $ Left (singleton $ ForeignError $ "Couldn't parse ReportData value from string: '' " <> show msg <> " ''")
      parseRegions ent@{cdnId, regions} = do
        rr ← readImpl regions
        id' ← readImpl cdnId
        pure {cdnId: id', regions: rr}
        where
          readString ∷ Foreign → F String
          readString = readImpl


type Report =
  { unit ∷ ReportUnit -- USD (for costs) | B (for traffic) | bps (for bandwidth)
  , data ∷ ReportData
  }
