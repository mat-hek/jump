{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module MotionDetector
  (Motion, MDState(MDState), Acceleration(Acceleration), initMDState, detectMotion)
where

import Utils

data Motion = Undetected | Stopped | Running | Jumping deriving (Eq, Show)

data MDState = MDState {
  motion::Motion,
  position::MotionPosition,
  lastChange::Int,
  lastx::Double,
  jumpIgnore::Double,
  scaler::MotionScaler
}
initMDState::MDState
initMDState = MDState Undetected None 0 0 0 initMotionScaler

data Acceleration = Acceleration {
  accx::Double, accy::Double, accz::Double,
  rota::Double, rotb::Double, rotg::Double
}

detectMotion::MDState -> Acceleration -> (Motion, MDState)
detectMotion state@MDState{motion=motion', scaler=scaler'} acc =
  case scaled of
    True -> doDetectMotion state'{position=None} acc
    False | motion' == Undetected -> state'
    False -> doDetectMotion state' acc
  |> \s -> (motion s, s)
  where
    (scaled, newScaler) = rescale scaler' acc
    state' = state{scaler=newScaler}

data MotionPosition = Top | Bottom | None deriving Eq

data MotionScaler = MotionScaler {zeroLvl::Double, motions::[Double]}
initMotionScaler::MotionScaler
initMotionScaler = MotionScaler 0 []

doDetectMotion::MDState -> Acceleration -> MDState
doDetectMotion
  (MDState motion' position' lastChange' lastx' lastJump'
    scaler'@MotionScaler{zeroLvl=zeroLvl'})
  Acceleration {accx = x}
  =
  case (position', motion') of
    (None, _)     | x < bottomLvl ->
                    nextState {motion = Running, position = Bottom}

    (_, Jumping)  | lastJump' > 0 ->
                    nextState{jumpIgnore = lastJump' - 1}
    (_, Jumping)  | motion' == Jumping && lastJump' == 0 ->
                    nextState{motion = Running}

    (_, _)        | (x > topJumpLvl || x < bottomJumpLvl) ->
                    nextState {motion = Jumping, jumpIgnore = jumpIgnore' - 1}

    (Top, _)      | x > topLvl ->
                    nextState {motion = Running}
    (Top, _)      | x < bottomLvl ->
                    nextState {motion = Running, position = Bottom}
    (Top, _)      | lastx' - x >= minDiff ->
                    nextState {motion = Running}
    (Top, _)      | lastx' - x < minDiff && lastChange' < maxUnchangedCnt ->
                    nextState {motion = Running, lastChange = lastChange' + 1}

    (Bottom, _)   | x < bottomLvl ->
                    nextState {motion = Running}
    (Bottom, _)   | x > topLvl ->
                    nextState {motion = Running, position = Top}
    (Bottom, _)   | x - lastx' >= minDiff ->
                    nextState {motion = Running}
    (Bottom, _)   | x - lastx' < minDiff && lastChange' < maxUnchangedCnt ->
                    nextState {motion = Running, lastChange = lastChange' + 1}

    _ -> nextState {motion = Stopped, position = None}
  where
    nextState = MDState motion' position' 0 x 0 scaler'

    topLvl = zeroLvl' + 2
    bottomLvl = zeroLvl' - 3
    topJumpLvl = zeroLvl' + 11
    bottomJumpLvl = zeroLvl' - 15
    jumpIgnore' = 8
    maxUnchangedCnt = 4
    minDiff = 4

rescale::MotionScaler -> Acceleration -> (Bool, MotionScaler)
rescale scaler'@(MotionScaler {motions = motions'}) Acceleration {accx = x}
  | length motions' < rescaleAfter = (False, scaler' {motions = x:motions'})
  | diff <= rescaleDiff && absDiff <= rescaleAbsDiff = (True, MotionScaler x newMotions)
  | otherwise = (False, scaler' {motions = newMotions})
  where
    newMotions = x:(init motions')
    absDiff = newMotions |> \(h:t) -> foldl1 ((+) . (subtract h) . abs) t
    diff = newMotions |> \(h:t) -> foldl1 ((+) . (subtract h)) t |> abs
    rescaleAfter = 20
    rescaleAbsDiff = 3
    rescaleDiff = 1
