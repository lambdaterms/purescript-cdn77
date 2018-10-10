module Main where

import Prelude

import Cdn77 (getCdnResourceDetails, listCdnResources, prefetch)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Types (CdnId(..))

main :: Effect Unit
main = launchAff_ do
  r <- listCdnResources {login, passwd}
  liftEffect $ log "List of resources"
  traceM r
  d <- getCdnResourceDetails {login, passwd, id}
  liftEffect $ log "Details of resource"
  traceM d
  p <- prefetch {login, passwd, cdn_id: id, url: urls}
  traceM p
  where
    login = "login"
    passwd = "passwd"
    id = CdnId 152266
    urls = []
