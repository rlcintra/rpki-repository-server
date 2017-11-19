{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RPKI.Repository.XML where

import qualified RPKI.Repository.Notification as N
import qualified RPKI.Repository.Snapshot as S
import Text.XML.HXT.Core

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

-- Snapshot

instance XmlPickler S.Snapshot where
  xpickle = xpSnapshot

xpSnapshot :: PU S.Snapshot
xpSnapshot =
  xpElem "snapshot" $
  xpAddFixedAttr "xmlns" "http://www.ripe.net/rpki/rrdp" $
  xpAddFixedAttr "version" "1" $
  xpWrap (\(si, s, ps) -> S.Snapshot si s ps,
          \s -> (S.sessionId s, S.serial s, S.publishs s)) $
  xpTriple (xpAttr "session_id" xpText)
           (xpAttr "serial" xpPrim)
           (xpList xpSPublish)

xpSPublish :: PU S.Publish
xpSPublish =
  xpElem "publish" $
  xpWrap (uncurry S.Publish,
          \p -> (S.uri (p :: S.Publish), S.object p)) $
  xpPair (xpAttr "uri" xpText)
         (xpText)
