{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.RedirectOf where

import           GHC.Exts
                 (Constraint)
import           GHC.TypeLits
                 (ErrorMessage (..), Nat, TypeError)
import           Servant.API.Sub
                 (type (:>))
import           Servant.API.Stream
                 (Stream)
import           Servant.API.UVerb
                 (UVerb)
import           Servant.API.Verbs
                 (StdMethod (..), Verb)
import           Servant.API.TypeLevel
                 (And, HasVerb, IsElem)
import           Servant.Links
                 (HasLink(..))

-- | With some old versions of the HTTP protocol, user-agents would sometimes change
-- the Method of their request, when receiving a 301 or 302 response code: for instance,
-- if a POST request is responded to with 301, it could be re-emitted as a GET request
-- to the endpoint specified in the Location header.
--
-- @RedirectLenciency@ specifies whether this behaviour should be supported:
-- If @RedirectLenient@ is used, we check that the @api@ parameter in @RedirectOf'@
-- can answer GET requests as well as the specific @method@ to be redirected.
-- If @RedirectStrict@ is used, we do not enforce that property.
data RedirectLeniency = RedirectLenient | RedirectStrict

data RedirectOf' (leniency :: RedirectLeniency) (status :: Nat) (method :: StdMethod) api where
   RedirectOf ::
    forall endpoint leniency status method api.
    ( IsElem endpoint api
    , HasLink endpoint
    , HandlesCorrectMethods leniency status method endpoint api
    ) => (forall a. MkLink endpoint a -> a)
      -> RedirectOf' leniency status method api

-- | Type alias using RedirectStrict by default
type RedirectOf = RedirectOf' 'RedirectStrict
-- Below is an extract from https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections
-- from which we define the logic for HTTP methods handling.
{--
Code 	Text 	                Method handling
-------------------------------------------
301 	Moved Permanently 	  GET methods unchanged. Others may or may not be changed to GET.
302 	Found 	              GET methods unchanged. Others may or may not be changed to GET.
303 	See Other 	          GET methods unchanged. Others changed to GET (body lost).
307 	Temporary Redirect 	  Method and body not changed
308 	Permanent Redirect 	  Method and body not changed.
--}

-- | @HandlesCorrectMethods leniency status method endpoint api@ checks that @endpoint@
-- serves all HTTP verbs required by @status@ and @method@. Should @leniency@ be
-- @RedirectLenient@, it also checks that @api@ serves GET when it's needed.
type HandlesCorrectMethods :: RedirectLeniency -> Nat -> StdMethod -> * -> * -> Constraint
type family HandlesCorrectMethods leniency status method endpoint api where
  HandlesCorrectMethods 'RedirectLenient 301 method endpoint api = HasGetAnd method endpoint api
  HandlesCorrectMethods 'RedirectStrict  301 method endpoint _   = HasVerb   method endpoint
  HandlesCorrectMethods 'RedirectLenient 302 method endpoint api = HasGetAnd method endpoint api
  HandlesCorrectMethods 'RedirectStrict  302 method endpoint _   = HasVerb   method endpoint
  HandlesCorrectMethods _                303 _      endpoint _   = HasVerb   'GET   endpoint
  HandlesCorrectMethods _                307 method endpoint _   = HasVerb   method endpoint
  HandlesCorrectMethods _                308 method endpoint _   = HasVerb   method endpoint
  -- Error cases
  HandlesCorrectMethods _ 300 _ _ _ = TypeError
    (     'Text "HTTP redirection status 300 (Multiple Choices) requires a HTTP body,"
    ':$$: 'Text "which is not yet supported by RedirectOf."
    ':$$: 'Text "Consider using Verb for this endpoint instead."
    )
  HandlesCorrectMethods _ 304 _ _ _ = TypeError
    (     'Text "HTTP redirection status 304 (Not Modified) requires several headers"
    ':$$: 'Text "in addition to \"Location\", which are not yet supported by RedirectOf."
    ':$$: 'Text "Consider using Verb for this endpoint instead."
    )
  HandlesCorrectMethods _ status _ _ _ = TypeError
    (     'Text "HTTP status code "
    ':<>: 'ShowType status
    ':<>: 'Text " is not a valid Redirection code"
    )

-- | Ensures that @endpoint@ has verb @method@, and that @api@ can serve GET
-- under the same URL as @endpoint@.
type HasGetAnd :: StdMethod -> * -> * -> Constraint
type family HasGetAnd method endpoint api where
  HasGetAnd 'GET   endpoint _   = HasVerb 'GET endpoint
  HasGetAnd method endpoint api = And
    (HasVerb method endpoint)
    (IsElem (TwinEndpoint endpoint) api)

-- | Computes the twin of @endpoint@, that is, the same endpoint
-- in which the method has been replaced by GET.
type TwinEndpoint :: * -> *
type family TwinEndpoint endpoint where
  TwinEndpoint (e :> sa)                          = e :> TwinEndpoint sa
  TwinEndpoint (Verb (_ :: StdMethod) s ct a)     = Verb 'GET s ct a
  TwinEndpoint (UVerb (_ :: StdMethod) ct as)     = UVerb 'GET ct as
  TwinEndpoint (Stream (_ :: StdMethod) s f ct a) = Stream 'GET s f ct a
  TwinEndpoint endpoint                           = TypeError
    (     'Text "Cannot compute twin endpoint of: "
    ':<>: 'ShowType endpoint
    )
