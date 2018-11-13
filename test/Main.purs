module Test.Main where

import Types

import Cdn77 (createStorage, deleteStorage, getCdnResourceDetails, listCdnResources, listStorages, storageDetails)
import Control.Applicative ((*>))
import Control.Monad.Except (ExceptT, except, runExceptT)
import Data.Argonaut (Json, jsonParser, stringify)
import Data.Either (Either(..))
import Data.Function (const)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Show (show)
import Debug.Trace (trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (lookupEnv)
import Prelude (Unit, bind, discard, pure, void, ($), (<$>), (<<<))
import Simple.JSON (class ReadForeign, writeImpl)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ do

  -- api credentials from environment variables
  login <- fromMaybe "" <$> (liftEffect $ lookupEnv "CDN77_API_LOGIN")
  passwd <- fromMaybe "" <$> (liftEffect $ lookupEnv "CDN77_API_PASSWORD")

  traceM login
  traceM passwd

  liftEffect $ log "== CDN Resources =="
  liftEffect $ log "List of resources"

  test listCdnResources { passwd, login}

  liftEffect $ log "Details of resource"
  test getCdnResourceDetails {id: cdn_id, passwd, login}


  liftEffect $ log "== Storage =="

  liftEffect $ log "Create storage"
  stE <- retTest createStorage { passwd, login, storage_location_id: StorageLocationId "push-30.cdn77.com", zone_name: "apiTest101"}

  case stE of
    Left x -> liftEffect $ log (show x) *> log "Failed t create storage. Storage tests disabled."
    Right st -> do
      liftEffect $ log $ "Created storage with id: " <> show st.storage.id

      liftEffect $ log "List of storages"
      test listStorages { passwd, login}

      liftEffect $ log "Storage details"
      test storageDetails { passwd, login, id: st.storage.id}

      liftEffect $ log "Delete storage"
      test deleteStorage { passwd, login, id: st.storage.id}

  -- liftEffect $ log "== Data =="

  -- liftEffect $ log "Prefetch"
  -- test prefetch {login, passwd, cdn_id, url: urls}

  -- liftEffect $ log "Purge"
  -- test purge {login, passwd, cdn_id, url: urls}

  -- liftEffect $ log "Purge All"
  -- test purgeAll {login, passwd, cdn_id }

  -- liftEffect $ log "== Data Queue =="

  -- liftEffect $ log "List Requests"
  -- test listRequests { type: requestType, cdn_id, login, passwd }

  -- liftEffect $ log "Get Request Details"
  -- test getRequestDetails { id: request_id, login, passwd }

  -- liftEffect $ log "List Request URL"
  -- test listRequestUrl { request_id, cdn_id, login, passwd }


  -- liftEffect $ log "== Report =="

  -- liftEffect $ log "Report details"
  -- test reportDetails { from: 1520950125, to:1540950125, type: Bandwidth, cdn_ids: [cdn_id], login, passwd }


  where

    test :: forall e a inp. (inp -> ExceptT e Aff a) -> inp -> Aff Unit
    test cmd = void <<< retTest cmd

    retTest :: forall e a inp. (inp -> ExceptT e Aff a) -> inp -> Aff (Either e a)
    retTest command params = do
      ret <- runExceptT $ command params
      traceM ret
      pure ret

    storage_id = StorageId "user_e6nixmh5"
    cdn_id = CdnId "153308"
    request_id = RequestId "0" -- no such request
    urls = ["README.md"]
    requestType = Purge
