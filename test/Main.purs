module Test.Main where

import Types

import Cdn77 (createCdnResource, createStorage, deleteCdnResource, deleteStorage, editCdnResource, getCdnResourceDetails, listCdnResources, listStorageLocations, listStorages, storageDetails)
import Control.Bind (when, (>>=))
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError, withExceptT)
import Data.Array (elem, null, unsafeIndex)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.HeytingAlgebra (not, (&&))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, pure, void, ($), (<<<))
import Simple.JSON (writeJSON)
import Test.Unit (Test, failure, success, suite, test, testSkip)
import Test.Unit.Main (runTest)



main :: Effect Unit
main = runTest $ suite "Testing CDN77 scenario" $ do

  cdn77T testSkip "Listing storage locations & creating storage with unique name. New storage becomes available in ~5 minutes!" $
    \ { login, passwd } -> do
      llog "Listing available storage locations:"
      locs <- l (listStorageLocations { login, passwd } )
      llog $ show locs

      when (null locs.storageLocations) $
        throwError "No available storage locations."

      let locId = (unsafePartial (unsafeIndex locs.storageLocations 0).id)

      llog "Creating storage for testing purposes"
      store <- l $ createStorage { passwd, login, storage_location_id: locId, zone_name: "apiTest101"}
      llog $ show store

  cdn77T testSkip  "Create, edit and manipulate storage" $
    \ { login, passwd } -> do
      llog "Listing available storage locations:"
      locs <- l (listStorageLocations { login, passwd } )
      llog $ show locs

      when (null locs.storageLocations) $
        throwError "No available storage locations."

      let locId = (unsafePartial (unsafeIndex locs.storageLocations 0).id)

      llog "Creating storage for testing purposes:"
      store <- l $ createStorage { passwd, login, storage_location_id: locId, zone_name: "apiTest102"}
      llog $ show store

      llog "Getting store details:"
      store2 <- l $ storageDetails { passwd, login, id: store.storage.id }
      llog $ show store2
      assert "Created storage and its details should equal"
        (store.storage == store2.storage)

      llog "Listing storages"
      stores <- l $ listStorages { passwd, login }
      llog $ show stores
      assert "Storages should contain newly created one"
        (store2.storage `elem` stores.storages)

      llog "Deleting storage"
      void $ l $ deleteStorage { passwd, login, id: store2.storage.id }

  cdn77T test  "Create, edit and manipulate resource" $
    \ { login, passwd } -> do
      llog "Listing available storage locations:"
      locs <- l (listStorageLocations { login, passwd } )
      llog $ show locs

      when (null locs.storageLocations) $
        throwError "No available storage locations."

      let locId = (unsafePartial (unsafeIndex locs.storageLocations 0).id)

      llog "Creating storage for testing purposes:"
      store <- l $ createStorage { passwd, login, storage_location_id: locId, zone_name: "apiTest103"}
      llog $ show store

      llog "Creating resource for testing purposes:"
      res <- l $ createCdnResource
        { gp_type: Whitelist
        , gp_countries: ["PL"]}
        { passwd, login
        , storage_id: store.storage.id
        , label: "apiTest103"
        , type: ResourceTypeStandard}
      let r1 = res.cdnResource
      llog $ writeJSON res

      res2 <- l $ getCdnResourceDetails
        { passwd, login, id: r1.id }

      assert "Created resources and its details should equal"
        (r1 == res2.cdnResource)

      llog "Listing resources:"
      ress <- l $ listCdnResources { passwd, login }
      llog $ writeJSON ress

      assert "Resources should contain newly created one"
        (r1 `elem` ress.cdnResources)

      llog "Editing resource:"
      res1e <- l $ editCdnResource
        { label: "changed", gp_type: Blacklist}
        { passwd, login, id: r1.id }
      llog $ writeJSON res1e

      assert "Edited resource asserts"
        (res1e.cdnResource.id == r1.id && res1e.cdnResource.label == "changed")

      llog "Deleting resource"
      dres <- l $ deleteCdnResource
        { passwd, login, id: r1.id }
      llog $ writeJSON dres

  where

    assertFalse msg b = assert msg (not b)
    assert msg = case _ of
      false -> throwError msg
      _ -> pure unit

    llog' :: String -> Aff Unit
    llog' = liftEffect <<< log

    llog :: forall e. String -> ExceptT e Aff Unit
    llog = lift <<< liftEffect <<< log

    lliftEffect :: forall a e. Effect a -> ExceptT e Aff a
    lliftEffect = lift <<< liftEffect

    l :: forall a. ExceptT ApiCallError Aff a -> ExceptT String Aff a
    l = withExceptT show

    asTest :: forall a. ExceptT String Aff a -> Test
    asTest act = runExceptT act >>= case _ of
      Left err -> failure err
      Right _ -> success


    cdn77T t name act = t name $ do
        mLogin <- liftEffect $ lookupEnv "CDN77_API_LOGIN"
        mPasswd <- liftEffect $ lookupEnv "CDN77_API_PASSWORD"
        case mLogin,mPasswd of
          Just login , Just passwd -> do
            llog' "Succesfully read CDN7 api credentials from the environment."
            asTest (act {login, passwd})
          _, _ -> failure "Couldn't read api credentials from environment. Provide CDN77_API_PASSWORD & CDN77_API_LOGIN variables"

  -- liftEffect $ log "Listing available storage locations:"
  
  -- listStorageLocations 


  -- traceM login
  -- traceM passwd

  -- liftEffect $ log "== CDN Resources =="
  -- liftEffect $ log "List of resources"

  -- test listCdnResources { passwd, login}

  -- liftEffect $ log "Details of resource"
  -- test getCdnResourceDetails {id: cdn_id, passwd, login}


  -- liftEffect $ log "== Storage =="

  -- liftEffect $ log "Create storage"
  -- stE <- retTest createStorage { passwd, login, storage_location_id: StorageLocationId "push-30.cdn77.com", zone_name: "apiTest101"}

  -- case stE of
  --   Left x -> liftEffect $ log (show x) *> log "Failed t create storage. Storage tests disabled."
  --   Right st -> do
  --     liftEffect $ log $ "Created storage with id: " <> show st.storage.id

  --     liftEffect $ log "List of storages"
  --     test listStorages { passwd, login}

  --     liftEffect $ log "Storage details"
  --     test storageDetails { passwd, login, id: st.storage.id}

  --     test2 createCdnResource { origin_port: 22 } { login, passwd, storage_id: st.storage.id, label: "PS-API-tests", type: ResourceTypeStandard  }

  --     liftEffect $ log "Delete storage"
  --     test deleteStorage { passwd, login, id: st.storage.id}

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


  -- where

    -- test :: forall e a inp. (inp -> ExceptT e Aff a) -> inp -> Aff Unit
    -- test cmd = void <<< retTest cmd

    -- test2 :: forall e a inp1 inp2. (inp1 -> inp2 -> ExceptT e Aff a) -> inp1 -> inp2 -> Aff Unit
    -- test2 cmd inp1 = void <<< retTest2 cmd inp1

    -- retTest2 :: forall e a inp1 inp2. (inp1 -> inp2 -> ExceptT e Aff a) -> inp1 -> inp2 -> Aff (Either e a)
    -- retTest2 command p1 p2  = do
    --   ret <- runExceptT $ command p1 p2
    --   traceM ret
    --   pure ret

    -- retTest :: forall e a inp. (inp -> ExceptT e Aff a) -> inp -> Aff (Either e a)
    -- retTest command params = do
    --   ret <- runExceptT $ command params
    --   traceM ret
    --   pure ret

    -- storage_id = StorageId "user_e6nixmh5"
    -- cdn_id = ResourceId "153308"
    -- request_id = RequestId "0" -- no such request
    -- urls = ["README.md"]
    -- requestType = Purge
