-- hdav.hs: WebDAV client
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

import qualified Data.ByteString.Char8 as BC8

import Paths_DAV (version)
import Data.Version (showVersion)
import Data.Maybe (fromMaybe, fromJust)

import Network (withSocketsDo)

import qualified System.Console.CmdArgs.Explicit as CA

import Network.Protocol.HTTP.DAV (getPropsAndContent, putContentAndProps, deleteContent, moveContent)

doCopy :: [(String, String)] -> IO ()
doCopy as = do
     let url1 = fromJust . lookup "sourceurl" $ as
     let url2 = fromJust . lookup "targeturl" $ as
     let sourceUsername = BC8.pack . fromMaybe "" . lookup "source-username" $ as
     let sourcePassword = BC8.pack . fromMaybe "" . lookup "source-password" $ as
     let targetUsername = BC8.pack . fromMaybe "" . lookup "target-username" $ as
     let targetPassword = BC8.pack . fromMaybe "" . lookup "target-password" $ as
     (p, b) <- getPropsAndContent url1 sourceUsername sourcePassword
     putContentAndProps url2 targetUsername targetPassword (p, b)

doDelete :: [(String, String)] -> IO ()
doDelete as = do
     let url = fromJust . lookup "url" $ as
     let username = BC8.pack . fromMaybe "" . lookup "username" $ as
     let password = BC8.pack . fromMaybe "" . lookup "password" $ as
     deleteContent url username password

doMove :: [(String, String)] -> IO ()
doMove as = do
     let url1 = fromJust . lookup "sourceurl" $ as
     let url2 = fromJust . lookup "targeturl" $ as
     let username = BC8.pack . fromMaybe "" . lookup "username" $ as
     let password = BC8.pack . fromMaybe "" . lookup "password" $ as
     moveContent url1 (BC8.pack url2) username password

dispatch :: String -> [(String, String)] -> IO ()
dispatch m as
    | m == "copy" = doCopy as
    | m == "move" = doMove as
    | m == "delete" = doDelete as
    | otherwise = fail "Unexpected condition."

showHelp :: IO ()
showHelp = print $ CA.helpText [] CA.HelpFormatDefault arguments

main :: IO ()
main = withSocketsDo $ do
    putStrLn $ "hDAV version " ++ showVersion version ++ ", Copyright (C) 2012  Clint Adams\n\
   \hDAV comes with ABSOLUTELY NO WARRANTY.\n\
   \This is free software, and you are welcome to redistribute it\n\
   \under certain conditions.\n\n"

    as <- CA.processArgs arguments
    if ("help","") `elem` as then showHelp else dispatch' as

    where dispatch' as = case lookup "mode" as of
                          Nothing -> showHelp
                          Just m -> dispatch m as

arguments :: CA.Mode [(String,String)]
arguments = CA.modes "hdav" [] "hdav WebDAV client" [
              (CA.mode "copy" [("mode", "copy")] "copy" (CA.flagArg (upd "sourceurl") "SOURCEURL") [
	          CA.flagReq ["source-username"] (upd "source-username") "USERNAME" "username for source URL"
		, CA.flagReq ["source-password"] (upd "source-password") "PASSWORD" "password for source URL"
		, CA.flagReq ["target-username"] (upd "target-username") "USERNAME" "username for target URL"
		, CA.flagReq ["target-password"] (upd "target-password") "PASSWORD" "password for target URL"
		, CA.flagHelpSimple (("help",""):)]) { CA.modeArgs = ([(CA.flagArg (upd "sourceurl") "SOURCEURL") { CA.argRequire = True }, (CA.flagArg (upd "targeturl") "TARGETURL") { CA.argRequire = True }], Nothing) }
              , (CA.mode "move" [("mode", "move")] "move" (CA.flagArg (upd "sourceurl") "SOURCEURL") [
	          CA.flagReq ["username"] (upd "username") "USERNAME" "username for source and target URL"
		, CA.flagReq ["password"] (upd "password") "PASSWORD" "password for source and target URL"
		, CA.flagHelpSimple (("help",""):)]) { CA.modeArgs = ([(CA.flagArg (upd "sourceurl") "SOURCEURL") { CA.argRequire = True }, (CA.flagArg (upd "targeturl") "TARGETURL") { CA.argRequire = True }], Nothing) }
              , (CA.mode "delete" [("mode", "delete")] "delete" (CA.flagArg (upd "url") "URL") [
	          CA.flagReq ["username"] (upd "username") "USERNAME" "username for URL"
		, CA.flagReq ["password"] (upd "password") "PASSWORD" "password for URL"
		, CA.flagHelpSimple (("help",""):)]) { CA.modeArgs = ([(CA.flagArg (upd "url") "URL") { CA.argRequire = True }], Nothing) }
	    ]
    where upd msg x v = Right $ (msg,x):v
