{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}

module OpenALHelloWorld where

import ClassyPrelude
import Sound.OpenAL as AL
import Sound.ALUT as AL

doItSoundman :: IO ()
doItSoundman = AL.withProgNameAndArgs AL.runALUT $ \_progName _args -> do
  mdev <- AL.openDevice Nothing
  case mdev of
    Nothing -> error "Cannot open device."
    Just dev -> do
      print =<< AL.alErrors
      mctxt <- AL.createContext dev []
      case mctxt of
        Nothing -> error "Cannot create context."
        Just ctxt -> do
          AL.currentContext AL.$= Just ctxt
          print =<< AL.alErrors
          AL.listenerPosition AL.$= (AL.Vertex3 0 0 1)
          AL.listenerVelocity AL.$= (AL.Vector3 0 0 0)
          AL.orientation AL.$= (AL.Vector3 0 0 1, AL.Vector3 0 1 0)
          src :: AL.Source <- AL.genObjectName
          AL.sourcePosition src AL.$= (AL.Vertex3 0 0 100)
          AL.sourceVelocity src AL.$= (AL.Vector3 0 0 0)
          AL.loopingMode src AL.$= AL.OneShot
          AL.distanceModel AL.$= AL.InverseDistance
          buf <- AL.createBuffer (AL.File "res/sound/africa-toto.wav")
          AL.buffer src AL.$= Just buf
          AL.play [src]
          void $ forM [0..] $ \x -> do
            AL.sourcePosition src AL.$= (AL.Vertex3 0 0 (1 + (x / 1000000)))
