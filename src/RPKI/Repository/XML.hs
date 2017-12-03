{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RPKI.Repository.XML where

import qualified RPKI.Repository.Notification as N
import qualified RPKI.Repository.Snapshot as S
import Text.XML.HXT.Core
import Text.XML.Expat.SAX
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L

import Debug.Trace as T

import Data.Char (isSpace)
import Data.Foldable (for_)

-- Notification

instance XmlPickler N.Notification where
  xpickle = xpNotification

instance XmlPickler N.Snapshot where
  xpickle = xpNSnaphot

instance XmlPickler N.Delta where
  xpickle = xpNDelta

xpNotification :: PU N.Notification
xpNotification =
  xpElem "notification" $
  xpAddFixedAttr "xmlns" "http://www.ripe.net/rpki/rrdp" $
  xpAddFixedAttr "version" "1" $
  xpWrap (\(si, s, ss, ds) -> N.Notification si s ss ds,
          \n -> (N.sessionId n, N.serial (n :: N.Notification), N.snapshot n, N.deltas n)) $
  xp4Tuple (xpAttr "session_id" xpText)
           (xpAttr "serial" xpPrim)
           (xpNSnaphot)
           (xpList xpickle)

xpNSnaphot :: PU N.Snapshot
xpNSnaphot =
  xpElem "snapshot" $
  xpWrap (uncurry N.Snapshot,
          \s -> (N.uri (s :: N.Snapshot), N.hash (s :: N.Snapshot))) $
  xpPair (xpAttr "uri" xpText)
         (xpAttr "hash" xpText)

xpNDelta :: PU N.Delta
xpNDelta =
  xpElem "delta" $
  xpWrap (uncurry3 N.Delta,
          \d -> (N.serial (d :: N.Delta), N.uri (d :: N.Delta), N.hash (d :: N.Delta))) $
  xpTriple (xpAttr "serial" xpPrim)
           (xpAttr "uri" xpText)
           (xpAttr "hash" xpText)

-- Snapshot (DOM)

-- instance XmlPickler S.Snapshot where
--   xpickle = xpSnapshot
--
-- xpSnapshot :: PU S.Snapshot
-- xpSnapshot =
--   xpElem "snapshot" $
--   xpAddFixedAttr "xmlns" "http://www.ripe.net/rpki/rrdp" $
--   xpAddFixedAttr "version" "1" $
--   xpWrap (\(si, s, ps) -> S.Snapshot si s ps,
--           \s -> (S.sessionId s, S.serial s, S.publishs s)) $
--   xpTriple (xpAttr "session_id" xpText)
--            (xpAttr "serial" xpPrim)
--            (xpList xpSPublish)
--
-- xpSPublish :: PU S.Publish
-- xpSPublish =
--   xpElem "publish" $
--   xpWrap (uncurry S.Publish,
--           \p -> (S.uri (p :: S.Publish), S.object p)) $
--   xpPair (xpAttr "uri" xpText)
--          (xpText)

-- Snapshot (SAX - hexpat)

loadSnapshot :: FilePath -> IO (Either String S.Snapshot)
loadSnapshot file = do
  bs <- C8L.readFile file
  return $ processSaxSnapshot $ parse defaultParseOptions bs

processSaxSnapshot :: [SAXEvent C8.ByteString C8.ByteString] -> Either String S.Snapshot
processSaxSnapshot [] = Left "No snapshot element found"
processSaxSnapshot (e:es) = S.Snapshot <$> sessionId <*> serial <*> publishs
  where sessionId = getSnapshotAttr (C8.pack "session_id") e
        serial = read . C8.unpack <$> getSnapshotAttr (C8.pack "serial") e
        getSnapshotAttr :: C8.ByteString -> SAXEvent C8.ByteString C8.ByteString -> Either String C8.ByteString
        getSnapshotAttr attr e =
          case e of
            StartElement elemName attrs ->
              if elemName /= C8.pack "snapshot"
              then Left "Element is not a snapshot"
              else maybe (Left $ "Attribute not found") (Right) (lookup attr attrs)
            _ -> Left "Root element is not snapshot."
        publishs = getPublishs es

getPublishs :: [SAXEvent C8.ByteString C8.ByteString] -> Either String [S.Publish]
getPublishs es
   | p == [] = Right []
   | otherwise = (:) <$> parsePublish  p <*> getPublishs rest
   where (p, rest) = break (isPublishEnd) $ dropWhile (isNotPublishStart) es
         isNotPublishStart (StartElement name _) = C8.unpack name /= "publish"
         isNotPublishStart _ = True
         isPublishEnd (EndElement name) = C8.unpack name == "publish"
         isPublishEnd _ = False

parsePublish :: [SAXEvent C8.ByteString C8.ByteString] -> Either String S.Publish
parsePublish es = S.Publish <$> uri <*> objectPayload
  where uri = getAttr (C8.pack "uri") attrs
        (StartElement _ attrs) = head es
        objectPayload = Right $ foldr C8.append C8.empty $ map getData $ filter isCharacterData es
        isCharacterData (CharacterData _) = True
        isCharacterData _ = False
        -- TODO: we probably can be more strict and improve performance here (removing these filters and assuming there will be only one character data
        getData (CharacterData d) = C8.filter (\c -> c /= '\n' &&  (not.isSpace) c) d
        getData _ = C8.empty

getAttr :: C8.ByteString -> [(C8.ByteString, C8.ByteString)] -> Either String C8.ByteString
getAttr name attrs =
   maybe (Left ("Attribute" ++ (C8.unpack name) ++ "not found.")) (Right) (lookup name attrs)
