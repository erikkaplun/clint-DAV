-- DAV.hs: WebDAV client library
-- Copyright © 2012  Clint Adams
--
-- vim: softtabstop=4:shiftwidth=4:expandtab
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings, ConstraintKinds, FlexibleContexts, QuasiQuotes #-}

module Network.Protocol.HTTP.DAV (
    DAVState
  , DAVContext(..)
  , getProps
  , getPropsAndContent
  , putContentAndProps
  , deleteContent
  , moveContent
  , makeCollection
  , module Network.Protocol.HTTP.DAV.TH
) where

import Network.Protocol.HTTP.DAV.TH

import Control.Applicative (liftA2)
import Control.Exception.Lifted (catchJust, finally, bracketOnError)
import Control.Lens ((.~), (^.))
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResourceBase, ResourceT, runResourceT, allocate)
import Control.Monad.Trans.State.Lazy (evalStateT, StateT, get, modify)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

import Network.HTTP.Conduit (httpLbs, parseUrl, applyBasicAuth, Request(..), RequestBody(..), Response(..), newManager, closeManager, ManagerSettings(..), def, HttpException(..))
import Network.HTTP.Types (hContentType, Method, Status, RequestHeaders, unauthorized401, conflict409)

import qualified Text.XML as XML
import Text.XML.Cursor (($/), (&/), element, node, fromDocument, checkName)
import Text.Hamlet.XML (xml)

import Data.CaseInsensitive (mk)

type DAVState m a = StateT (DAVContext m) (ResourceT m) a

initialDS :: String -> B.ByteString -> B.ByteString -> ManagerSettings -> IO (DAVContext a)
initialDS u username password s = do
    mgr <- newManager s
    req <- parseUrl u
    return $ DAVContext [] req [] mgr Nothing username password

closeDS :: DAVContext a -> IO ()
closeDS = closeManager . _httpManager

withDS :: MonadResourceBase m => String -> B.ByteString -> B.ByteString -> DAVState m a -> m a
withDS url username password f = runResourceT $ do
    (_, ds) <- allocate (initialDS url username password def) closeDS
    evalStateT f ds

davRequest :: MonadResourceBase m => Method -> RequestHeaders -> RequestBody (ResourceT m) -> DAVState m (Response BL.ByteString)
davRequest meth addlhdrs rbody = do
    ctx <- get
    let req = (ctx ^. baseRequest) { method = meth, requestHeaders = (mk "User-Agent", "hDav 9.0"):addlhdrs, requestBody = rbody }
    let authreq = applyBasicAuth (ctx ^. basicusername) (ctx ^. basicpassword) req
    resp <- lift (catchJust (matchStatusCodeException unauthorized401)
                            (httpLbs req (ctx ^. httpManager))
                            (\_ -> httpLbs authreq (ctx ^. httpManager)))
    return resp

matchStatusCodeException :: Status -> HttpException -> Maybe ()
matchStatusCodeException want (StatusCodeException s _)
    | s == want = Just ()
    | otherwise = Nothing
matchStatusCodeException _ _ = Nothing

emptyBody :: RequestBody m
emptyBody = RequestBodyLBS BL.empty

xmlBody :: XML.Document -> RequestBody m
xmlBody = RequestBodyLBS . XML.renderLBS XML.def

getOptions :: MonadResourceBase m => DAVState m ()
getOptions = do
    optresp <- davRequest "OPTIONS" [] emptyBody
    let meths = (B.splitWith (==(fromIntegral . fromEnum) ',') . fromMaybe B.empty . lookup "Allow" . responseHeaders) optresp
    let cclass = (B.splitWith (==(fromIntegral . fromEnum) ',') . fromMaybe B.empty . lookup "DAV" . responseHeaders) optresp
    modify (complianceClasses .~ cclass)
    modify (allowedMethods .~ meths)

lockResource :: MonadResourceBase m => Bool -> DAVState m ()
lockResource nocreate = do
    let ahs' = [(hContentType, "application/xml; charset=\"utf-8\""), (mk "Depth", "0"), (mk "Timeout", "Second-300")]
    let ahs = if nocreate then (mk "If-Match", "*"):ahs' else ahs'
    lockresp <- davRequest "LOCK" ahs (xmlBody locky)
    let hdrtoken = (lookup "Lock-Token" . responseHeaders) lockresp
    modify (lockToken .~ hdrtoken)

unlockResource :: MonadResourceBase m => DAVState m ()
unlockResource = do
    d <- get
    case _lockToken d of
        Nothing -> return ()
	Just tok -> do let ahs = [(mk "Lock-Token", tok)]
                       _ <- davRequest "UNLOCK" ahs emptyBody
                       modify (lockToken .~ Nothing)

supportsLocking :: DAVContext a -> Bool
supportsLocking = liftA2 (&&) ("LOCK" `elem`) ("UNLOCK" `elem`) . _allowedMethods

getAllProps :: MonadResourceBase m => DAVState m XML.Document
getAllProps = do
    let ahs = [(hContentType, "application/xml; charset=\"utf-8\"")]
    propresp <- davRequest "PROPFIND" ahs (xmlBody propname)
    return $ (XML.parseLBS_ def . responseBody) propresp

getContent :: MonadResourceBase m => DAVState m (Maybe B.ByteString, BL.ByteString)
getContent = do
    resp <- davRequest "GET" [] emptyBody
    let ct = lookup (hContentType) (responseHeaders resp)
    return $ (ct, responseBody resp)

putContent :: MonadResourceBase m => (Maybe B.ByteString, BL.ByteString) -> DAVState m ()
putContent (ct, body) = do
    d <- get
    let ahs' = fromMaybe [] (fmap (return . (,) (mk "If") . parenthesize) (d ^. lockToken))
    let ahs = ahs' ++ fromMaybe [] (fmap (return . (,) (hContentType)) ct)
    _ <- davRequest "PUT" ahs (RequestBodyLBS body)
    return ()

delContent :: MonadResourceBase m => DAVState m ()
delContent = do
    _ <- davRequest "DELETE" [] emptyBody
    return ()

mvContent :: MonadResourceBase m => B.ByteString -> DAVState m ()
mvContent newurl = do
    let ahs = [ (mk "Destination", newurl) ]
    _ <- davRequest "MOVE" ahs emptyBody
    return ()

mkCol :: MonadResourceBase m => DAVState m ()
mkCol = do
    _ <- davRequest "MKCOL" [] emptyBody
    return ()

parenthesize :: B.ByteString -> B.ByteString
parenthesize x = B.concat ["(", x, ")"]

putProps :: MonadResourceBase m => XML.Document -> DAVState m ()
putProps props = do
    d <- get
    let ah' = (hContentType, "application/xml; charset=\"utf-8\"")
    let ahs = ah':fromMaybe [] (fmap (return . (,) (mk "If") . parenthesize) (_lockToken d))
    _ <- davRequest "PROPPATCH" ahs ((RequestBodyLBS . props2patch) props) -- FIXME: should diff and remove props from target
    return ()

props2patch :: XML.Document -> BL.ByteString
props2patch = XML.renderLBS XML.def . patch . props . fromDocument
   where
       props cursor = map node (cursor $/ element "{DAV:}response" &/ element "{DAV:}propstat" &/ element "{DAV:}prop" &/ checkName (not . flip elem blacklist))
       patch prop = XML.Document (XML.Prologue [] Nothing []) (root prop) []
       root [] = propertyupdate []
       root prop = propertyupdate
           [ XML.NodeElement $ XML.Element "D:set" Map.empty
	     [ XML.NodeElement $ XML.Element "D:prop" Map.empty prop ]
	   ]
       propertyupdate = XML.Element "D:propertyupdate" (Map.fromList [("xmlns:D", "DAV:")])
       blacklist = [ "{DAV:}creationdate"
                   , "{DAV:}displayname"
                   , "{DAV:}getcontentlength"
                   , "{DAV:}getcontenttype"
                   , "{DAV:}getetag"
                   , "{DAV:}getlastmodified"
                   , "{DAV:}lockdiscovery"
                   , "{DAV:}resourcetype"
                   , "{DAV:}supportedlock"
                   ]

getProps :: String -> B.ByteString -> B.ByteString -> IO XML.Document
getProps url username password = withDS url username password getAllProps

getPropsAndContent :: String -> B.ByteString -> B.ByteString -> IO (XML.Document, (Maybe B.ByteString, BL.ByteString))
getPropsAndContent url username password = withDS url username password $ do
    getOptions
    o <- get
    when (supportsLocking o) (lockResource True)
    (do props <- getAllProps
        body <- getContent
        return (props, body)) `finally` when (supportsLocking o) (unlockResource)

putContentAndProps :: String -> B.ByteString -> B.ByteString -> (XML.Document, (Maybe B.ByteString, BL.ByteString)) -> IO ()
putContentAndProps url username password (p, b) = withDS url username password $ do
    getOptions
    o <- get
    when (supportsLocking o) (lockResource False)
    (do putContent b
        putProps p) `finally` when (supportsLocking o) (unlockResource)

deleteContent :: String -> B.ByteString -> B.ByteString -> IO ()
deleteContent url username password = withDS url username password $ do
    getOptions
    o <- get
    let lock = when (supportsLocking o) (lockResource False)
    -- a successful delete destroys locks, so only unlock on error
    let unlock = when (supportsLocking o) (unlockResource)
    bracketOnError lock (\_ -> unlock) (\_ -> delContent)

moveContent :: String -> B.ByteString -> B.ByteString -> B.ByteString -> IO ()
moveContent url newurl username password = withDS url username password $
    mvContent newurl

-- | Creates a WebDAV collection, which is similar to a directory.
--
-- Returns False if the collection could not be made due to an intermediate
-- collection not existing. (Ie, collection /a/b/c/d cannot be made until
-- collection /a/b/c exists.)
makeCollection :: String -> B.ByteString -> B.ByteString -> IO Bool
makeCollection url username password = withDS url username password $
    catchJust 
        (matchStatusCodeException conflict409)
        (mkCol >> return True)
        (\_ -> return False)



propname :: XML.Document
propname = XML.Document (XML.Prologue [] Nothing []) root []
    where
        root = XML.Element "D:propfind" (Map.fromList [("xmlns:D", "DAV:")]) [xml|
<D:allprop>
|]

locky :: XML.Document
locky = XML.Document (XML.Prologue [] Nothing []) root []
    where
        root = XML.Element "D:lockinfo" (Map.fromList [("xmlns:D", "DAV:")]) [xml|
<D:lockscope>
  <D:exclusive>
<D:locktype>
  <D:write>
<D:owner>Haskell DAV user
|]

