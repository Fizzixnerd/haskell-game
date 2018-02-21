{-# LANGUAGE ScopedTypeVariables #-}

module OpenALHelloWorld where

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
          AL.sourceGain src AL.$= 1
          AL.sourcePosition src AL.$= (AL.Vertex3 0 0 1)
          AL.sourceVelocity src AL.$= (AL.Vector3 0 0 0)
          AL.loopingMode src AL.$= AL.OneShot
          buf <- AL.createBuffer (File "res/sound/a2002011001-e02.wav")
          AL.buffer src AL.$= Just buf
          AL.play [src]
          AL.sleep 5
