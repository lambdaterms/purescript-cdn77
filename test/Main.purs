module Test.Main where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, getField, jsonEmptyObject, toObject, (.?), (:=), (~>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Test.QuickCheck (class Arbitrary, Result(..), (<?>), (===))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Unsafe.Coerce (unsafeCoerce)


data Proxy a = Proxy

main :: Effect Unit
main = runTest $ do
  suite "Argonaut's Json vs Simple.JSON's Foreign equivalence tests" do

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

  where

    testAssert msg t = test msg (t >>= Assert.assert msg)

    json2foreignObject :: TestObjR -> Result
    json2foreignObject x0@{a:a0, b:b0, c:c0,d:d0} =
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
