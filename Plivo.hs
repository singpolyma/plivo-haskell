module Plivo (
	callAPI,
	APIError(..),
	-- * Enpoints
	MakeCall(..),
	makeCall
) where

import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.String (IsString, fromString)
import UnexceptionalIO (fromIO, runUnexceptionalIO)
import Control.Exception (fromException)
import Control.Error (EitherT, fmapLT, throwT, runEitherT)
import Network.URI (URI(..), URIAuth(..))
import Network.Http.Client (withConnection, establishConnection, sendRequest, buildRequest, http, setAccept, setContentType, Response, receiveResponse, RequestBuilder, inputStreamBody, emptyBody, getStatusCode, setAuthorizationBasic, setContentLength)
import qualified Network.Http.Client as HttpStreams
import Blaze.ByteString.Builder (Builder)
import System.IO.Streams (OutputStream, InputStream, fromLazyByteString)
import System.IO.Streams.Attoparsec (parseFromStream, ParseException(..))
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)
import Network.HTTP.Types.URI (renderQuery)
import Network.HTTP.Types.Method (Method)
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
		answer_url :: URI,
		answer_method :: Maybe Method,
		ring_url :: Maybe URI,
		ring_method :: Maybe Method,
		hangup_url :: Maybe URI,
		hangup_method :: Maybe Method,
		fallback_url :: Maybe URI,
		fallback_method :: Maybe Method,
		caller_name :: Maybe String,
		send_digits :: Maybe String,
		send_on_preanswer :: Maybe Bool,
		time_limit :: Maybe Int,
		hangup_on_ring :: Maybe Int,
		machine_detection :: Maybe String,
		machine_detection_time :: Maybe Int,
		sip_headers :: [(String,String)],
		ring_timeout :: Maybe Int
	} deriving (Show, Eq)

-- | Helper for constructing simple 'MakeCall'
makeCall ::
	String    -- ^ from
	-> String -- ^ to
	-> URI    -- ^ answer_url
	-> MakeCall
makeCall from to answer_url = MakeCall from to answer_url
	Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
	Nothing Nothing Nothing Nothing Nothing [] Nothing

instance ToJSON MakeCall where
	toJSON (MakeCall from to answer_url answer_method ring_url ring_method
	        hangup_url hangup_method fallback_url fallback_method caller_name
	        send_digits send_on_preanswer time_limit hangup_on_ring
	        machine_detection machine_detection_time sip_headers ring_timeout
		) = object $ catMaybes [
			Just $ s"from" .= from,
			Just $ s"to" .= to,
			Just $ s"answer_url" .= show answer_url,
			fmap (s"answer_method" .=) answer_method,
			fmap ((s"ring_url" .=) . show) ring_url,
			fmap (s"ring_method" .=) ring_method,
			fmap ((s"hangup_url" .=) . show) hangup_url,
			fmap (s"hangup_method" .=) hangup_method,
			fmap ((s"fallback_url" .=) . show) fallback_url,
			fmap (s"fallback_method" .=) fallback_method,
			fmap (s"caller_name" .=) caller_name,
			fmap (s"send_digits" .=) send_digits,
			fmap (s"send_on_preanswer" .=) send_on_preanswer,
			fmap (s"time_limit" .=) time_limit,
			fmap (s"hangup_on_ring" .=) hangup_on_ring,
			fmap (s"machine_detection" .=) machine_detection,
			fmap (s"machine_detection_time" .=) machine_detection_time,
			fmap (s"sip_headers" .=) (sipFmt sip_headers),
			fmap (s"ring_timeout" .=) ring_timeout
		]
		where
		sipFmt [] = Nothing
		sipFmt xs = Just $ intercalate "," $ map (\(k,v) -> k ++ "=" ++ v) xs

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
	oneShotHTTP HttpStreams.POST uri req' (inputStreamBody bodyStream) responseHandler
	where
	body = encode payload

get :: (QueryLike a, FromJSON b) => URI -> RequestBuilder () -> a -> IO (Either APIError b)
get uri req payload = do
	let req' = do
		setAccept (BS8.pack "application/json")
		req
	oneShotHTTP HttpStreams.GET uri' req' emptyBody responseHandler
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

oneShotHTTP :: HttpStreams.Method -> URI -> RequestBuilder () -> (OutputStream Builder -> IO ()) -> (Response -> InputStream ByteString -> IO b) -> IO b
oneShotHTTP method uri req body handler = do
	req' <- buildRequest $ do
		http method (BS8.pack $ uriPath uri)
		req
	withConnection (establishConnection url) $ \conn -> do
		sendRequest conn req' body
		receiveResponse conn handler
	where
	url = BS8.pack $ show uri -- URI can only have ASCII, so should be safe
