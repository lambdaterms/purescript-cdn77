module Main where

import Prelude

import Cdn77 (getCdnResourceDetails, getRequestDetails, listCdnResources, listRequestUrl, listRequests, prefetch, purge, purgeAll)
import Control.Monad.Except (ExceptT, runExceptT)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Types (CdnId(..), RequestId(..), RequestType(..))

main :: Effect Unit
main = launchAff_ do

  liftEffect $ log "== CDN Resources =="

  liftEffect $ log "List of resources"
  test listCdnResources { passwd, login}

  liftEffect $ log "Details of resource"
  test getCdnResourceDetails {id: cdn_id, passwd, login}

  liftEffect $ log "== Data =="

  liftEffect $ log "Prefetch"
  test prefetch {login, passwd, cdn_id, url: urls}

  liftEffect $ log "Purge"
  test purge {login, passwd, cdn_id, url: urls}

  liftEffect $ log "Purge All"
  test purgeAll {login, passwd, cdn_id }

  liftEffect $ log "== Data Queue =="

  liftEffect $ log "List Requests"
  test listRequests { type: requestType, cdn_id, login, passwd }

  liftEffect $ log "Get Request Details"
  test getRequestDetails { id: request_id, login, passwd }

  liftEffect $ log "List Request URL"
  test listRequestUrl { request_id, cdn_id, login, passwd }

  where

    test :: forall e a inp. (inp -> ExceptT e Aff a) -> inp -> Aff Unit
    test command params = do
      ret <- runExceptT $ command params
      traceM ret

    login = "login"
    passwd = "password"
    cdn_id = CdnId "153308"
    request_id = RequestId "0" -- no such request
    urls = []
    requestType = Purge
