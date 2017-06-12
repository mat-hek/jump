{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
import Test.HUnit

import Utils
import MotionDetector.Internal as MD
import WSServer.Internal

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()


tests = test [
    "MotionDetector.detectMotion" ~: [
      "should change position from Top to Bottom once border is reached" ~: let
          acc = zeroAcceleration {accx=(-6)}
          ms = MotionScaler 0 $ [1..20] |> fmap (\_ -> 0)
          ms' = ms {motions=(accx acc):([1..19] |> fmap (\_ -> 0))}
          s = MDState {
            motion=Running, position=Top, lastChange=0,
            lastx=(-3), jumpIgnore=0, scaler=ms
          }
          s' = s {position=Bottom, lastx=accx acc, scaler=ms'}
          in (Running, s') ~=? (detectMotion s acc)
    ],
    "WSServer.parseAcceleration" ~: [
      "should parse proper acceleration" ~:
        (parseAcceleration "1;2;3;4;5;6") ~=? (Just $ MD.Acceleration 1 2 3 4 5 6),
      "should fail on invalid input" ~: (parseAcceleration "abc") ~=? Nothing
    ]
  ]
