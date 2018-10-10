module Types where

import Control.Applicative (pure)
import Control.Bind ((>>=))
import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.List.NonEmpty (singleton)
import Data.Nullable (Nullable)
import Data.Semigroup ((<>))
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- [TODO]
type Timestamp = String

data ApiCallError = RequestError String
                  | ServerReponseError String
                  | ResourceError String

newtype CdnId = CdnId Int

derive newtype instance writeForeignCdnId ∷ WriteForeign CdnId
derive newtype instance readForeignCdnId ∷ ReadForeign CdnId

newtype UrlId = UrlId Int

derive newtype instance writeForeignUrlId ∷ WriteForeign UrlId
derive newtype instance readForeignUrlId ∷ ReadForeign UrlId

newtype RequestId = RequestId Int

derive newtype instance writeForeignRequestId ∷ WriteForeign RequestId
derive newtype instance readForeignRequestId ∷ ReadForeign RequestId

newtype StorageId = StorageId Int

derive newtype instance writeForeignStorageId ∷ WriteForeign StorageId
derive newtype instance readForeignStorageId ∷ ReadForeign StorageId


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
    On → writeImpl "1"
    Off → writeImpl "0"

instance readForeignSwitch ∷ ReadForeign Switch where
  readImpl frn = readImpl frn >>= case _ of
    "0" → pure On
    "1" → pure Off
    x   → except $ Left (singleton $ ForeignError $ "Couldn't match switch value. Should be '0' or '1'. Was: '" <> x <> "'")

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


type CDNResource =
  { id ∷ CdnId  -- Your CDN Id. See how to retrieve a list of your cdns including their ids.
  , label ∷ String -- Your own label of a CDN Resource.
  , origin_url ∷ String -- URL of your content source (Origin). Doesn't have to be set when CDN Storage Id is set (that means instead of using your own URL you use our CDN Storage).
  , cname ∷ String -- Domain name that will redirect to our CDN server.
  , cache_expiry ∷ Int -- In minutes. Valid values: '10' | '10800' | '11520' | '12960' | '1440' | '14440' | '15840' | '17280' | '2160' | '240' | '2880' | '30' | '4320' | '5760' | '60' | '720' | '7200' | '8640'
  , url_signing_on ∷ Switch -- Allow generating of secured links with expiration time. Content is not available without valid token. Valid values: '0' | '1'
  , url_signing_key ∷ Nullable String -- Key (hash) for signing URLs.
  , instant_ssl ∷ Switch -- Set to 1 if you want to have a SSL certificate for every CNAME for free.
  , type ∷ String -- Valid values: 'standard' | 'video'
  , storage_id ∷ StorageId -- Storage Id. See available Storages and their Id in the list of storages. Set to 0 if you want to disable CDN Storage. Ignore query string (qs_status) is set to 1 when you enable CDN Storage as Origin.
  , qs_status ∷ Switch -- By default the entire URL is treated as a separate cacheable item. If you want to override this, set qs_status to '1', otherwise to '0'. If you have CDN Storage set as Origin, qs_status is automatically set to 1. Valid values: '0' | '1'
  , setcookie_status ∷ Switch -- To cache Set-Cookies responses, set this to '1' (disabled by default). Valid values: '0' | '1'
  , other_cnames ∷ Array String  -- Array. Maximum length of array is 9.
  , mp4_pseudo_on ∷ Switch -- Allow streaming video in mp4 format. For video, a CDN Resource is automatically enabled. Valid values: '0' | '1'
  }

type CDNResourceDetails = CDNResource


type ApiRequest =
  { id ∷ RequestId -- Queued request ID. You will retrieve this ID in your purge/prefetch request.
  , cdn_id ∷ CdnId -- CDN Id. See how to retrieve a list of your CDNs including their Ids.
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
