-- | Module responsible for recognizing motion basing on accelerometer data
module MotionDetector
  (Motion(..), MDState(MDState), initMDState, Acceleration(Acceleration), detectMotion)
where
import MotionDetector.Internal
