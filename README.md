# purescript-cdn77

Purescript library providing native typesafe bindings to CDN77's http web api.

https://client.cdn77.com/support/api/version/2.0/


# Library status & important info

## Original documentation and API

The original https api documentaion is located at:  
https://client.cdn77.com/support/api/version/2.0/  
Please refer to it whenever you find the code and its description not clear.

## Implementation Status

Only some parts of the API are implemented. These are:  
- CDN Resources
- Storage
- Storage Location
- Data Management
- Data Queue
- Reports  

If you need some other part consider implementing it on your own on creating a PR. The code is kept simple and it should be almost mechanical. If you don't feel confident enough post your needs as a github's issue.

## Stability

Library is not mature and may be unstable. Please report any issues via Github's tracker. Additionally should you have any ideas on how to improve any aspect of the library, we're open for suggestions and PR's.

# Getting Started

## Installation

Only Node.js is supported platform.
Intall the library in your project using bower.

```
bower install lambdaterms/purescript-cdn77
```

## Usage


Here's a simple example of how the library is used:

```purescript

import Control.Alt ((<$>))
import Control.Apply ((<*>))
import Control.Bind ((>>=))
import Control.Monad.Except (ExceptT, lift, runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (lookupEnv)
import Prelude (Unit, bind, discard, pure, ($), (<<<))
import Simple.JSON (writeJSON)

import Node.Network.Cdn77 (ApiCallError, listCdnResources, listStorageLocations, listStorages)


main :: Effect Unit
main = launchAff_ do
  mLogin <- liftEffect $ lookupEnv "CDN77_API_LOGIN"
  mPasswd <- liftEffect $ lookupEnv "CDN77_API_PASSWORD"
  case {login: _, passwd: _} <$> mLogin <*> mPasswd of
    Nothing -> liftEffect $ log "Couldn't read api credentials from environment. Provide CDN77_API_PASSWORD & CDN77_API_LOGIN variables"
    Just {login, passwd} -> reportErrors $ runExceptT do
      llog "Succesfully read CDN7 api credentials from the environment."
      llog "Listing available storage locations:"
      locs <- listStorageLocations { login, passwd }
      llog $ show locs

      llog "Listing storages"
      storesResp <- listStorages { passwd, login }
      llog $ writeJSON storesResp

      llog "Listing resources"
      ressResp <- listCdnResources { passwd, login }
      llog $ writeJSON ressResp

  where
    reportErrors :: Aff (Either ApiCallError Unit) -> Aff Unit
    reportErrors aff = aff >>= case _ of
      (Left err) -> liftEffect $ log $ show err
      _          -> pure unit

    llog :: forall e. String -> ExceptT e Aff Unit
    llog = lift <<< liftEffect <<< log
              
```

For more involved examples see include test suites.

## Tests

There is a small test suite using sftp bindings to perform all of the operations provided by the library.
Notice that you have to provide your CDN77 api credentials as environmental variables.

Remember to install dev dependencies.

```
pulp test
```

# Credits

Library is co-created and funded by [`λ-terms`](https://github.com/lambdaterms/)

# License & copyrights

See LICENSE file.
