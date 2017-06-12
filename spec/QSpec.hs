{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
import Test.QuickCheck

import Data.List
import qualified Data.Text as Text

import WSServer.Internal
import MotionDetector.Internal
import Utils

arb = arbitrary

instance Arbitrary Acceleration where
  arbitrary = Acceleration <$> arb <*> arb <*> arb <*> arb <*> arb <*> arb

parseAccelerationTest :: Acceleration -> Bool
parseAccelerationTest acc@(Acceleration x y z a b g) = [x, y, z, a, b, g]
  |> fmap show |> intercalate ";" |> Text.pack |> parseAcceleration
  == Just acc

newtype RescalableList = RescalableList [Double] deriving Show
instance Arbitrary RescalableList where
  arbitrary =
    [1..20] |> fmap (\_ -> choose (-0.01, 0.01)) |> sequence |> fmap RescalableList

rescaleTest :: RescalableList -> Bool
rescaleTest (RescalableList (x:l)) =
  (rescale initMotionScaler {motions=l'} zeroAcceleration {accx=x})
    == (True, MotionScaler {motions=motions', zeroLvl=avg})
  where
    l' = 0:l
    motions' = x:(init l')
    avg = sum motions' / fromIntegral (length motions')

main = do
  putStrLn "WSServer.parseAcceleration"
  quickCheck parseAccelerationTest
  putStrLn "MotionDetector.rescale"
  quickCheck rescaleTest
  return ()
