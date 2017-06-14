-- | Module containing helper functions to separate IO functions from non-IO ones
module WSHelper (WSAction(Send), handleEvent, handleMessage, dumbReceive) where
import WSHelper.Internal
