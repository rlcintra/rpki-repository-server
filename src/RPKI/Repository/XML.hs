{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RPKI.Repository.XML where

import Conduit
import Crypto.Hash
import Crypto.Hash.Conduit (sinkHash)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.XML.Types as X
import Text.XML.HXT.Core
import Text.XML.Expat.SAX
import Text.XML.Stream.Render (renderBytes, def)
import qualified RPKI.Repository.Delta as D
import qualified RPKI.Repository.Notification as N
import qualified RPKI.Repository.Snapshot as S

import Debug.Trace as T

import Data.Char (isSpace)
import Data.Foldable (for_)

-- Notification

loadNotification :: FilePath -> IO (Either String N.Notification)
loadNotification p = do
  notifications <- runX $ xunpickleDocument xpNotification [withRemoveWS yes] p
  case listToMaybe notifications of
    Just notification -> return $ Right notification
    Nothing           -> return $ Left "Notification file could not be parsed."

writeNotification :: N.Notification -> FilePath -> IO ()
writeNotification n p =
  do
    _ <- runX $ constA n >>> xpickleDocument xpNotification [] p
    return ()

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
processSaxSnapshot (e:es) =
  case e of
    XMLDeclaration _ _ _ -> processSaxSnapshot es
    StartElement elemName attrs ->
         if elemName /= C8.pack "snapshot"
         then Left "Element is not a snapshot"
         else S.Snapshot <$> sessionId <*> serial <*> publishs
         where sessionId = getSnapshotAttr (C8.pack "session_id")
               serial = read . C8.unpack <$> getSnapshotAttr (C8.pack "serial")
               getSnapshotAttr :: C8.ByteString -> Either String C8.ByteString
               getSnapshotAttr attr = maybe (Left $ "Attribute not found") (Right) (lookup attr attrs)
               publishs = getPublishs es
    _ -> Left "Error parsing snapshot."

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
   maybe (Left ("Attribute" ++ C8.unpack name ++ "not found.")) Right (lookup name attrs)

-- Snapshot (SAX - Conduit - Write)

writeSnapshot :: S.Snapshot -> FilePath -> IO String
writeSnapshot s f = do
  digest <- runConduitRes $ sourceSnapshot s .| renderBytes def .| sinkSnapshotFile .| sinkHash
  return $ show (digest :: Digest SHA256)
  where sinkSnapshotFile = passthroughSink (sinkFileBS f) (\_ -> return ())

sourceSnapshot :: MonadIO m => S.Snapshot -> Source m X.Event
sourceSnapshot s = do
  yieldMany snapshotHeader
  yieldMany $ concatMap publishEvent $ S.publishs s
  yieldMany snapshotFooter

  where snapshotHeader =
          [
            X.EventBeginDocument,
            X.EventBeginElement snapshotName [ (toName "session_id", [X.ContentText $ decodeUtf8 $ S.sessionId s]),
                                               (toName "serial", [X.ContentText $ T.pack . show $ S.serial s]),
                                               (toName "xmlns", [X.ContentText "http://www.ripe.net/rpki/rrdp"]) ]
          ]
        publishEvent p =
          [
            X.EventBeginElement publishName [(toName "uri", [X.ContentText $ decodeUtf8 $ S.uri p])],
            X.EventContent $ X.ContentText $ decodeUtf8 $ S.object p,
            X.EventEndElement publishName
          ]
        snapshotFooter =
          [
            X.EventEndElement snapshotName,
            X.EventEndDocument
          ]

snapshotName, publishName :: X.Name
snapshotName = toName "snapshot"
publishName = toName "publish"

toName :: T.Text -> X.Name
toName name = X.Name name Nothing Nothing


-- Delta (Write)

writeDelta :: D.Delta -> FilePath -> IO String
writeDelta delta fp = do
  print $ "here " ++ fp -- TODO: REMOVE
  digest <- runConduitRes $ sourceDelta delta .| renderBytes def .| sinkDeltaFile .| sinkHash
  return $ show (digest :: Digest SHA256)
  where sinkDeltaFile = passthroughSink (sinkFileBS fp) (\_ -> return ())

sourceDelta :: MonadIO m => D.Delta -> Source m X.Event
sourceDelta delta = do
  yieldMany deltaHeader
  yieldMany $ concatMap publishEvent $ D.publishs delta
  yieldMany $ concatMap withdrawEvent $ D.withdraws delta
  yieldMany deltaFooter

  where deltaHeader =
          [
            X.EventBeginDocument,
            X.EventBeginElement deltaName [ (toName "session_id", [X.ContentText $ T.pack $ D.sessionId delta]),
                                               (toName "serial", [X.ContentText $ T.pack . show $ D.serial delta]),
                                               (toName "xmlns", [X.ContentText "http://www.ripe.net/rpki/rrdp"]),
                                               (toName "version", [X.ContentText "1"])]
          ]
        publishEvent :: D.Publish -> [X.Event]
        publishEvent p =
          [
            X.EventBeginElement publishName $
              catMaybes [Just (toName "uri", [X.ContentText $ decodeUtf8 $ D.uri (p :: D.Publish)]),
                         case D.hash (p :: D.Publish) of
                           Nothing -> Nothing
                           Just h  -> Just (toName "hash", [X.ContentText $ decodeUtf8 h])],
            X.EventContent $ X.ContentText $ decodeUtf8 $ D.object p,
            X.EventEndElement publishName
          ]
        withdrawEvent w =
          [
            X.EventBeginElement withdrawName [(toName "uri", [X.ContentText $ decodeUtf8 $ D.uri (w :: D.Withdraw)]),
                                              (toName "hash", [X.ContentText $ decodeUtf8 $ D.hash (w :: D.Withdraw)])],
            X.EventEndElement withdrawName
          ]
        deltaFooter =
          [
            X.EventEndElement deltaName,
            X.EventEndDocument
          ]

deltaName, withdrawName :: X.Name
deltaName = toName "delta"
withdrawName = toName "withdraw"
