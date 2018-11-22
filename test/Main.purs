module Test.Main where

import Types

import Cdn77 (addStorageCdnResources, createCdnResource, createCdnResource_, createStorage, deleteCdnResource, deleteStorage, editCdnResource, getCdnResourceDetails, getRequestDetails, listCdnResources, listRequestUrl, listRequests, listStorageLocations, listStorages, prefetch, purge, purgeAll, reportDetails, storageDetails)
import Control.Alt ((<|>))
import Control.Applicative ((*>))
import Control.Bind ((>>=))
import Control.Monad.Except (ExceptT(..), lift, runExceptT, throwError, withExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, getField, jsonEmptyObject, toObject, (.?), (:=), (~>))
import Data.Array (elem, find, head)
import Data.Either (Either(..), either)
import Data.Eq (class Eq, (==))
import Data.Function (identity)
import Data.Functor ((<#>))
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Nullable (Nullable, notNull, null)
import Data.Show (class Show, show)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign)
import Node.Network.SftpClient (list, mkdir, fastPut, runSftpSession) as Sftp
import Node.Process (lookupEnv)
import Prelude (Unit, bind, discard, pure, ($), (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, E, read, readJSON, readJSON_, write, writeJSON)
import Test.QuickCheck (class Arbitrary, Result(..), (<?>), (===))
import Test.Unit (Test, TestSuite, failure, success, suite, test, testSkip)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


main :: Effect Unit
main = runTest $ jsonForeignTestSuite *> cdn77ApiTestSuite


cdn77ApiTestSuite :: TestSuite
cdn77ApiTestSuite = do
  let resName = "PsApiTest_v_0_1_0_resource"
      resTemp = "PsApiTest_v_0_1_0_resource_temp102"
      storeName = "PsApiTest_v_0_1_0_store"
      storeTemp = "PsApiTest_v_0_1_0_store_temp102"

  cdn77T test "Listing storage locations & creating permament storage and resource for testing purposes. New storage becomes available in ~5 minutes!" $
    \ { login, passwd } -> do
      llog $ login <> " " <> passwd
      llog "Listing available storage locations:"
      locs <- l (listStorageLocations { login, passwd } )
      llog $ show locs

      locId <- errOnNothing "No available storage locations." $ head locs.storageLocations <#> _.id

      llog "Listing storages"
      storesResp <- l $ listStorages { passwd, login }
      llog $ writeJSON storesResp


      store <- case find (\s -> s.zone_name == storeName) storesResp.storages of
        Just x -> llog "Permament test storage already exists" *> pure x
        Nothing -> do
          llog "Creating permament storage for testing purposes"
          llog $ writeJSON locId
          storeResp <- l $ createStorage { passwd, login, storage_location_id: locId, zone_name: storeName}
          llog $ show storeResp
          pure $ storeResp.storage

      llog "Listing resources"
      ressResp <- l $ listCdnResources { passwd, login }
      llog $ writeJSON ressResp

      case find (\r -> r.label == resName) ressResp.cdnResources of
        Just x -> llog "Permament test storage already exists" *> pure unit
        Nothing -> do
            llog "Creating permament resource for testing purposes"
            resResp <- l $ createCdnResource_ { passwd, login, label: resName, type: ResourceTypeStandard, storage_id: store.id }
            llog $ writeJSON resResp


  cdn77T test  "Create, edit and manipulate storage" $
    \ { login, passwd } -> do

      llog "Listing available storage locations:"
      locs <- l (listStorageLocations { login, passwd } )
      llog $ show locs

      locId <- errOnNothing "No available storage locations." $ head locs.storageLocations <#> _.id

      llog "Creating temporary storage for testing purposes:"
      store <- l $ createStorage { passwd, login, storage_location_id: locId, zone_name: storeTemp }
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

      llog "Binding permament test resource to this storage."

      llog "Listing resources"
      ressResp <- l $ listCdnResources { passwd, login }
      llog $ writeJSON ressResp

      case find (\r -> r.label == resName) ressResp.cdnResources of
        Just res -> do
          llog "Found permament test resource. Proceeding "
          addResourceResp <- l $ addStorageCdnResources { login, passwd, id: store2.storage.id, cdn_ids: [res.id]}
          llog $ show addResourceResp

          llog "Reverting new storage bind."

          llog "Listing storages"
          storesResp <- l $ listStorages { passwd, login }
          llog $ writeJSON storesResp

          res1e <- case find (\s -> s.zone_name == storeName) storesResp.storages of
            Just store' -> do
              llog "Setting permament test storage as the resoure storage"
              l $ editCdnResource { storage_id: store'.id } { passwd, login, id: res.id }
            Nothing -> do
              llog "Setting origin url: lambdaterms.com in place of storage"
              l $ editCdnResource { storage_id: disableStorage, origin_url:"lambdaterms.com"} { passwd, login, id: res.id }
          llog $ writeJSON res1e

        Nothing -> do
          llog "Couldn't find permament test resource. Create one using one of prepared tests. Skipping binding resource test."

      llog "Deleting temporary storage"
      deleteResp <- l $ deleteStorage { passwd, login, id: store2.storage.id }
      llog $ show deleteResp

  cdn77T test "Create, edit and manipulate resource" $
    \ { login, passwd } -> do
      llog "Listing availabte storage locations:"
      locs <- l (listStorageLocations { login, passwd } )
      llog $ show locs

      locId <- errOnNothing "No available storage locations." $ head locs.storageLocations <#> _.id

      llog "Creating temporary storage for testing purposes:"
      storeResp <- l $ createStorage { passwd, login, storage_location_id: locId, zone_name: storeTemp }
      llog $ show storeResp

      llog $ "Creating temporary resource for testing purposes (label: "<> resTemp <> " ):"
      res <- l $ createCdnResource
        { gp_type: Whitelist
        , gp_countries: ["PL"]}
        { passwd, login
        , storage_id: storeResp.storage.id
        , label: resTemp
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

      llog "Editing resource and disabling storage:"
      let storeTempChanged = storeTemp <> "changed"
      res1e <- l $ editCdnResource
        { label: storeTempChanged, gp_type: Blacklist, storage_id: disableStorage, origin_url:"lambdaterms.com"}
        { passwd, login, id: r1.id }
      llog $ writeJSON res1e

      assert "Edited resource asserts"
        (res1e.cdnResource.id == r1.id && res1e.cdnResource.label == storeTempChanged)

      llog "Deleting temporary storage"
      dsResp <- l $ deleteStorage
        { passwd, login, id: storeResp.storage.id }
      llog $ writeJSON dsResp

      llog "Deleting temporary resource"
      dres <- l $ deleteCdnResource
        { passwd, login, id: r1.id }
      llog $ writeJSON dres

  cdn77T test "Get storage creds & manage file with sftp & make requests & check it" $
    \ { login, passwd } -> do

      llog "Listing storages"
      storesResp <- l $ listStorages { login, passwd }
      llog $ show storesResp
      llog $ "Looking for storage with zone_name: " <> storeName

      store <- errOnNothing "Storage not found. You can create it with running one of prepared tests." $
                 find (\s -> s.zone_name == storeName) storesResp.storages

      llog $ "Found matching storage with id: " <> show store.id


      llog "Starting sftp session"
      let config =
            { host: store.credentials.host
            , port: "22"
            , username: store.credentials.user
            , password: store.credentials.pass }

          remoteRoot = "/www/"
          remoteDir = remoteRoot <> storeName
          fileName = "bower.json"
          remotePath = remoteDir <> "/" <> fileName


      ExceptT $ Sftp.runSftpSession config $ do
        liftAff $ llog' $ "Connected to sftp://" <> config.host
        ls <- Sftp.list remoteRoot
        liftAff $ llog' $ "Listing " <> remoteRoot <> " directory:\n" <> show ls
        case find (\fi -> fi.type == "d" && fi.name == storeName) ls of
          Nothing -> do
            liftAff $ llog' $ "Creating directory: " <> storeName
            Sftp.mkdir {path: remoteDir, recursive: false}
          Just d -> liftAff $ llog' $ "Directory exists: " <> storeName

        liftAff $ llog' $ "Uploading file: " <> fileName
        Sftp.fastPut {local: fileName, remote: remotePath }
        pure $ Right unit
      llog "Sftp session finished."

      llog "Listing resources:"
      ress <- l $ listCdnResources { passwd, login }
      llog $ writeJSON ress

      llog $ "Looking for resource with added storage with id: " <> show store.id
      res <- case find (\res -> res.storage_id == Just store.id) ress.cdnResources of
        Nothing -> do
          llog "Such resource not found. Let's create one."
          res <- l $ createCdnResource_
            { passwd, login
            , storage_id: store.id
            , label: resName
            , type: ResourceTypeStandard}
          llog $ writeJSON res
          pure res.cdnResource
        Just r -> do
          llog "Found such resource."
          pure r

      llog $ "Prefetching " <> remotePath
      preres <- l $ prefetch {login, passwd, cdn_id: res.id, url: [remotePath] }
      llog $ show preres

      llog "List prefetch requests made to current resource"
      reqs <- l $ listRequests { login, passwd, type: Prefetch, cdn_id: res.id }
      llog $ writeJSON reqs

      llog $ "Purging " <> remotePath
      purgeres <- l $ purge {login, passwd, cdn_id: res.id, url: [remotePath] }
      llog $ show purgeres

      llog $ "Purging all"
      purgeAllRes <- l $ purgeAll {login, passwd, cdn_id: res.id}
      llog $ show purgeAllRes

      llog "List purge requests made to current resource"
      purges <- l $ listRequests { login, passwd, type: Purge, cdn_id: res.id }
      llog $ writeJSON purges

      llog "Trying to get details and urls from request"
      req <- errOnNothing "Couldn't find any request" $ (head purges.requests <|> head reqs.requests)
      llog $ "Request details for request_id: " <> show req.id
      reqDetRes <- l $ getRequestDetails { id: req.id, login, passwd }
      llog $ writeJSON reqDetRes

      llog $ "Requesting urls for request_id: " <> show req.id
      reqUrlsDetRes <- l $ listRequestUrl { cdn_id: res.id, request_id: req.id, login, passwd }
      llog $ writeJSON reqUrlsDetRes

  cdn77T test "Getting & showing report" $
    \ { login, passwd } -> do
      let from = "1520950125"
          to = "1650950125"

      llog $ "Looking for resource with test id: " <> resName
      ress <- l $ listCdnResources { passwd, login }
      llog $ writeJSON ress
      res <- errOnNothing "Couldn't not find. Cannot report." $ find (\res -> res.label == resName || res.label == resName <> zone) ress.cdnResources

      llog "Showing report for Bandwidth"
      repBandRes <- l $ reportDetails { login, passwd, type: Bandwidth, cdn_ids: [res.id], from, to }
      llog $ show repBandRes

      llog "Showing report for Costs"
      repCostRes <- l $ reportDetails { login, passwd, type: Costs, cdn_ids: [res.id], from, to }
      llog $ show repCostRes

      llog "Showing report for Traffic"
      repTfcRes <- l $ reportDetails { login, passwd, type: Traffic, cdn_ids: [res.id], from, to }
      llog $ show repTfcRes

      llog "Showing report for HitMiss"
      repHtmsRes <- l $ reportDetails { login, passwd, type: Costs, cdn_ids: [res.id], from, to }
      llog $ show repHtmsRes



  where

    assertFalse msg b = assert msg (not b)
    assert msg = case _ of
      false -> throwError msg
      _ -> pure unit

    llog :: forall e. String -> ExceptT e Aff Unit
    llog = lift <<< liftEffect <<< log

    lliftEffect :: forall a e. Effect a -> ExceptT e Aff a
    lliftEffect = lift <<< liftEffect

    errOnNothing :: forall a. String -> Maybe a -> ExceptT String Aff a
    errOnNothing msg = case _ of
      Nothing -> throwError msg
      Just r -> pure r

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


maybeNullableJsonSuite :: TestSuite
maybeNullableJsonSuite = suite "Testing behaviour of Maybe (Nullable a)" $ do
  test "write nothing" $ do
    let str = writeJSON (testR (Nothing :: Maybe (Nullable Int)))
    llog' $ str
    Assert.assert "Should't be included." ( str == writeJSON testUndef )

  test "write just" $ do
    let str = writeJSON (testR (Just (notNull 10)))
    llog' str
    Assert.assert "Should equal" ( str == writeJSON (testPresent 10))

  test "write null" $ do
    let str = writeJSON (testR (Just (null :: Nullable Int)))
    llog' str
    Assert.assert "Should equal" ( str == writeJSON testNull)

  test "read nothing" $ do
    let j = readJSON readTestUndef :: E ({ a :: Maybe (Nullable Int), i :: Int, s :: String})
    llog' $ show j
    Assert.assert "Should parse and and equal with test case" ( j == Right (testR Nothing) )

  test "read just" $ do
    let j = (readJSON_ readTestJust) :: Maybe ({ a :: Maybe (Nullable Int), i :: Int, s :: String})
    llog' $ show j
    Assert.assert "Should parse and and equal with test case" ( j == Just (testR (Just (notNull 10))) )

  test "read null" $ do
    let j = (readJSON_ readTestNull) :: Maybe ({ a :: Maybe (Nullable Int), i :: Int, s :: String})
    llog' $ show j
    Assert.assert "Should parse and and equal with test case" ( j == Just (testR (Just null)) )

  test "read maybe" $ do
    let j = (readJSON_ readTestNull) :: Maybe ({ a :: Maybe Int, i :: Int, s :: String})
    llog' $ show j
    Assert.assert "Should parse and and equal with test case" ( j == Just testMaybe)

  where

    readTestUndef = "{\"s\":\"foo\",\"i\":1}"
    readTestNull = "{\"s\":\"foo\",\"i\":1,\"a\":null}"
    readTestJust = "{\"s\":\"foo\",\"i\":1,\"a\":10}"

    testR :: forall a. WriteForeign a => Maybe (Nullable a) -> { a :: Maybe (Nullable a), i :: Int, s :: String}
    testR a = {a, i: 1, s: "foo"}

    testUndef :: {i :: Int, s :: String}
    testUndef = {i: 1, s: "foo"}

    testPresent :: forall a. a -> {a :: a, i :: Int, s :: String}
    testPresent a = {a, i: 1, s: "foo"}

    testMaybe :: { a :: Maybe Int, i :: Int, s :: String}
    testMaybe = {a: Nothing, i: 1, s: "foo"}

    testNull :: {a :: Nullable Int, i :: Int, s :: String}
    testNull = {a: null, i: 1, s: "foo"}



jsonForeignTestSuite :: TestSuite
jsonForeignTestSuite = suite "Argonaut's Json vs Simple.JSON's Foreign equivalence tests" do

    test "a -> Json -> Foreign -> a -------- (Int)"
      (quickCheck (json2foreignSimple (Proxy :: Proxy Int)))

    test "a -> Json -> Foreign -> a -------- (String)"
      (quickCheck (json2foreignSimple (Proxy :: Proxy String)))

    test "a -> Json -> Foreign -> a -------- (Number)"
      (quickCheck (json2foreignSimple (Proxy :: Proxy Number)))

    test "a -> Json -> Foreign -> a -------- (Array Number)"
      (quickCheck (json2foreignSimple (Proxy :: Proxy (Array Number))))

    test "a -> Foreign -> Json -> a -------- (Int)"
      (quickCheck (foreign2jsonSimple (Proxy :: Proxy Int)))

    test "a -> Foreign -> Json -> a -------- (String)"
      (quickCheck (foreign2jsonSimple (Proxy :: Proxy String)))

    test "a -> Foreign -> Json -> a -------- (Number)"
      (quickCheck (foreign2jsonSimple (Proxy :: Proxy Number)))

    test "a -> Foreign -> Json -> a -------- (Array Number)"
      (quickCheck (foreign2jsonSimple (Proxy :: Proxy (Array Number))))

    test "x@{ a :: Int, b :: Number, c :: String } -> Foreign -> Json -> x"
      (quickCheck foreign2jsonObject)

    test "x@{ a :: Int, b :: Number, c :: String, d :: Array String } -> Json -> Foreign -> x"
      (quickCheck foreign2jsonObject)

    test "Array a -> Json -> Foreign ->  Array a -------- "
      (quickCheck (json2foreignArray (Proxy :: Proxy TestObj)))
  where

    testAssert msg t = test msg (t >>= Assert.assert msg)

    json2foreignObject :: TestObjR -> Result
    json2foreignObject x0 =
      let
        json1 = encodeJson (TestObj x0)
        x1E = read (unsafeCoerce json1)
      in
        case x1E of
          Left err -> Failed $ "Error: " <> show err <> " can't read from encoded json"
          Right x1 -> x1 === x0


    foreign2jsonObject :: {a :: Int, b :: Number, c :: String} -> Result
    foreign2jsonObject x0@{a:a0, b:b0, c:c0} =
      let
        frn1 = write x0
        json1 = unsafeCoerce frn1
      in
       either Failed identity do
         obj <- withError (show x0 <> " is not an Object according to Argonaut") $ toObject json1
         a <- getField obj "a"
         b <- getField obj "b"
         c <- getField obj "c"
         pure $ (a == a0 && b == b0 && c == c0) <?>
           ("Did not hold: " <> show a <> "==" <> show a0 <> " && "
            <> show b <> "==" <> show b0 <> " && "
            <> show c <> "==" <> show c0)

      where
        withError str = case _ of
          Nothing -> Left str
          Just x  -> Right x


    json2foreignArray :: forall a. Arbitrary a => Show a => Eq a => EncodeJson a => DecodeJson a => ReadForeign a => WriteForeign a => Proxy a -> Array a -> Result
    json2foreignArray p x0 =
      let
        json1 = encodeJson  x0
        x1E = readArray (unsafeCoerce json1)
      in
        case x1E of
          Left err -> Failed $ "Error: " <> show err <> " can't read from encoded json"
          Right x1 -> x1 === x0

      where
        readArray :: ReadForeign a => Foreign -> E (Array a)
        readArray = read


    json2foreignSimple :: forall a. Arbitrary a => Show a => Eq a => EncodeJson a => DecodeJson a => ReadForeign a => WriteForeign a => Proxy a -> a -> Result
    json2foreignSimple p x0 =
      let
        json1 = encodeJson x0
        x1E = read (unsafeCoerce json1)
      in
        case x1E of
          Left err -> Failed $ "Error: " <> show err <> " can't read from encoded json"
          Right x1 -> x1 === x0

    foreign2jsonSimple :: forall a. Arbitrary a => Show a => Eq a => EncodeJson a => DecodeJson a => ReadForeign a => WriteForeign a => Proxy a -> a -> Result
    foreign2jsonSimple p x0 =
      let
        frn1 = write x0
        x1E = decodeJson (unsafeCoerce frn1)
      in
        case x1E of
          Left err -> Failed $ "Error: " <> show err <> " can't read from encoded json"
          Right x1 -> x1 === x0

type TestObjR =
  { a :: Int
  , b :: String
  , c :: Number
  , d :: Array Number
  }
newtype TestObj = TestObj TestObjR


derive newtype instance readForeignTestObj :: ReadForeign TestObj
derive newtype instance writeForeignTestObj :: WriteForeign TestObj
derive newtype instance arbitrartyForeignTestObj :: Arbitrary TestObj
derive newtype instance showForeignTestObj :: Show TestObj
derive newtype instance eqForeignTestObj :: Eq TestObj

instance decodeJsonTestObj :: DecodeJson TestObj where
  decodeJson json = do
    obj <- decodeJson json
    a <- obj .? "a"
    b <- obj .? "b"
    c <- obj .? "c"
    d <- obj .? "d"
    pure $ TestObj { a, b, c, d }

instance encodeJsonTestObj :: EncodeJson TestObj where
  encodeJson (TestObj obj)
     = "a" := obj.a
    ~> "b" := obj.b
    ~> "c" := obj.c
    ~> "d" := obj.d
    ~> jsonEmptyObject

llog' :: String -> Aff Unit
llog' = liftEffect <<< log
