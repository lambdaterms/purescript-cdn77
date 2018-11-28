module Node.Network.Cdn77.Utils where

import Control.Category ((<<<))
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Simple.JSON (class WriteForeign, writeImpl)

foreign import urlEncodedImpl ∷ (String → String → Tuple String (Maybe String)) →  Foreign → Array (Tuple String (Maybe String))

foreign import coerceJsonHelperImpl ∷ Foreign -> Foreign

urlEncoded ∷ ∀ r. WriteForeign { | r } ⇒ { | r } → FormURLEncoded
urlEncoded = wrap <<< urlEncodedImpl buildTuple <<< writeImpl
  where
    buildTuple k v = Tuple k (Just v)


errorType ∷ ∀ a b c. (a → c) → Either a b → Either c b
errorType f = case _ of
  Left x → Left (f x)
  Right x → Right x
