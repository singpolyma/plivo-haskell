module Plivo (
	callAPI,
	APIError(..),
	-- * Enpoints
	MakeCall(..)
) where

import Data.String (IsString, fromString)
import UnexceptionalIO (fromIO, runUnexceptionalIO)
import Control.Exception (fromException)
import Control.Error (EitherT, fmapLT, throwT, runEitherT)
import Network.URI (URI(..), URIAuth(..))
import Network.Http.Client (withConnection, establishConnection, sendRequest, buildRequest, http, Method(POST, GET), setAccept, setContentType, Response, receiveResponse, RequestBuilder, inputStreamBody, emptyBody, getStatusCode, setAuthorizationBasic, setContentLength)
import Blaze.ByteString.Builder (Builder)
import System.IO.Streams (OutputStream, InputStream, fromLazyByteString)
import System.IO.Streams.Attoparsec (parseFromStream, ParseException(..))
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)
import Network.HTTP.Types.URI (renderQuery)
import Network.HTTP.Types.Status (Status)
import Data.Aeson (encode, ToJSON, toJSON, FromJSON, fromJSON, Result(..), object, (.=), json', Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString.Char8 as BS8 -- eww

s :: (IsString a) => String -> a
s = fromString

class Endpoint a where
	endpoint :: String -> RequestBuilder () -> a -> IO (Either APIError Value)

-- | The endpoint to place an outbound call
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

instance Endpoint MakeCall where
	endpoint aid = post (apiCall ("Account/" ++ aid ++ "/Call/"))

-- | Call a Plivo API endpoint
--
-- You must wrap your app in a call to 'OpenSSL.withOpenSSL'
callAPI :: (Endpoint a) =>
	String    -- ^ AuthID
	-> String -- ^ AuthToken
	-> a      -- ^ Endpoint data
	-> IO (Either APIError Value)
callAPI aid atok = endpoint aid auth
	where
	-- These should be ASCII
	auth = setAuthorizationBasic (BS8.pack aid) (BS8.pack atok)

-- Construct URIs

baseURI :: URI
baseURI = URI "https:" (Just $ URIAuth "" "api.plivo.com" "") "/v1/" "" ""

apiCall :: String -> URI
apiCall ('/':path) = apiCall path
apiCall path = baseURI { uriPath = uriPath baseURI ++ path }

-- HTTP requests

post :: (ToJSON a, FromJSON b) => URI -> RequestBuilder () -> a -> IO (Either APIError b)
post uri req payload = do
	let req' = do
		setAccept (BS8.pack "application/json")
		setContentType (BS8.pack "application/json")
		setContentLength (LZ.length body)
		req
	bodyStream <- fromLazyByteString body
	oneShotHTTP POST uri req' (inputStreamBody bodyStream) responseHandler
	where
	body = encode payload

get :: (QueryLike a, FromJSON b) => URI -> RequestBuilder () -> a -> IO (Either APIError b)
get uri req payload = do
	let req' = do
		setAccept (BS8.pack "application/json")
		req
	oneShotHTTP GET uri' req' emptyBody responseHandler
	where
	uri' = uri { uriQuery = BS8.unpack $ renderQuery True (toQuery payload)}

data APIError = APIParamError | APIAuthError | APINotFoundError | APIParseError | APIRequestError Status | APIOtherError
	deriving (Show, Eq)

responseHandler :: (FromJSON a) => Response -> InputStream ByteString -> IO (Either APIError a)
responseHandler resp i = runUnexceptionalIO $ runEitherT $ do
	case getStatusCode resp of
		code | code >= 200 && code < 300 -> return ()
		400 -> throwT APIParamError
		401 -> throwT APIAuthError
		404 -> throwT APINotFoundError
		code -> throwT $ APIRequestError $ toEnum code
	v <- fmapLT (handle . fromException) $ fromIO $ parseFromStream json' i
	case fromJSON v of
		Success a -> return a
		Error _ -> throwT APIParseError
	where
	handle (Just (ParseException _)) = APIParseError
	handle _ = APIOtherError

oneShotHTTP :: Method -> URI -> RequestBuilder () -> (OutputStream Builder -> IO ()) -> (Response -> InputStream ByteString -> IO b) -> IO b
oneShotHTTP method uri req body handler = do
	req' <- buildRequest $ do
		http method (BS8.pack $ uriPath uri)
		req
	withConnection (establishConnection url) $ \conn -> do
		sendRequest conn req' body
		receiveResponse conn handler
	where
	url = BS8.pack $ show uri -- URI can only have ASCII, so should be safe
