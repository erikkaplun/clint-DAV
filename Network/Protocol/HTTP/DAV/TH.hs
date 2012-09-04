-- DAV.hs: WebDAV client library
-- Copyright © 2012  Clint Adams
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

{-# LANGUAGE TemplateHaskell #-}

module Network.Protocol.HTTP.DAV.TH where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import Network.HTTP.Conduit (Manager, Request)

data DAVContext a = DAVContext {
    _allowedMethods :: [B.ByteString]
  , _baseRequest :: Request a
  , _httpManager :: Manager
  , _lockToken :: Maybe B.ByteString
  , _basicusername :: B.ByteString
  , _basicpassword :: B.ByteString
}
makeLenses ''DAVContext
