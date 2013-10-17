module Plivo where

import Data.Maybe
import Data.String (IsString, fromString)
import UnexceptionalIO (UnexceptionalIO, fromIO)
import Control.Error (EitherT, hoistEither, fmapLT, throwT)
import Network.URI (URI(..), URIAuth(..))
import Network.HTTP (simpleHTTP, mkRequest, replaceHeader, rqBody, Request(..), RequestMethod(GET,POST,DELETE), HeaderName(HdrContentType, HdrContentLength, HdrUserAgent), Header(..), Response(..))
import Network.Stream (ConnError(ErrorMisc))
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)
import Network.HTTP.Types.URI (renderQuery)
import Network.HTTP.Types.Status (Status)
import Data.Aeson (encode, ToJSON, toJSON, object, (.=))
import qualified Data.ByteString.Char8 as BS8 -- eww
import qualified Data.ByteString.Lazy as LZ

s :: (IsString a) => String -> a
s = fromString

data MakeCall = MakeCall {
		from :: String,
		to :: String,
		answer_url :: URI
	} deriving (Show, Eq)

instance ToJSON MakeCall where
	toJSON (MakeCall {
			from = from,
			to = to,
			answer_url = answer_url
		}) = object [
			s"from" .= from,
			s"to" .= to,
			s"answer_url" .= show answer_url
		]

makeCall :: String -> String -> MakeCall -> Request LZ.ByteString
makeCall aid atok payload =
	auth aid atok $
	post (apiCall ("Account/" ++ aid ++ "/Call/")) payload

-- Do HTTP

data APIError = APIParamError | APIAuthError | APINotFoundError | APIOtherError Status | APIConnError ConnError
	deriving (Show, Eq)

http :: Request LZ.ByteString -> EitherT APIError UnexceptionalIO LZ.ByteString
http req = do
	res <- fmapLT APIConnError $ hoistEither =<< fmapLT (ErrorMisc . show) (fromIO $ simpleHTTP req)
	case rspCode res of
		(2,_,_) -> return undefined
		(4,0,0) -> throwT APIParamError
		(4,0,1) -> throwT APIAuthError
		(4,0,4) -> throwT APINotFoundError
		(x,y,z) -> throwT $ APIOtherError $ toEnum (x*100 + y*10 + z)

-- Construct URIs

baseURI :: URI
baseURI = URI "https:" (Just $ URIAuth "" "api.plivo.com" "") "/v1/" "" ""

apiCall :: String -> URI
apiCall ('/':path) = apiCall path
apiCall path = baseURI { uriPath = (uriPath baseURI) ++ path }

-- Authentication

auth ::
	String    -- ^ Auth ID
	-> String -- ^ Auth token
	-> Request a
	-> Request a
auth aid atok req = req {
		rqURI = (rqURI req) { uriAuthority = Just $ auth {
			uriUserInfo = aid ++ "@" ++ atok
		}}
	}
	where
	auth = fromMaybe (URIAuth "" "" "") (uriAuthority $ rqURI req)

-- Create HTTP requests

post :: (ToJSON a) => URI -> a -> Request LZ.ByteString
post uri payload =
	Request {
		rqURI = uri,
		rqMethod = POST,
		rqBody = bytes,
		rqHeaders = [
			Header HdrUserAgent "Haskell Plivo Module",
			Header HdrContentType "application/json",
			Header HdrContentLength (show $ LZ.length bytes)
		]
	}
	where
	bytes = encode payload

get :: (QueryLike a) => URI -> a -> Request LZ.ByteString
get uri payload =
	Request {
		rqURI = uri { uriQuery = BS8.unpack $ renderQuery True (toQuery payload)},
		rqMethod = GET,
		rqBody = LZ.empty,
		rqHeaders = [
			Header HdrUserAgent "Haskell Plivo Module",
			Header HdrContentLength "0"
		]
	}

delete :: (QueryLike a) => URI -> a -> Request LZ.ByteString
delete uri payload = (get uri payload) { rqMethod = DELETE }
