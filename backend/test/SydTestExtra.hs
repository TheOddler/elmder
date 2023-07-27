module SydTestExtra where

import Test.Syd (HList, SetupFunc, TestDefM, setupAroundWith')

-- | Use multiple outers when creating an inner
-- I'll be contributing this function to SydTest, but until it's been merged I'll keep this here.
setupAroundWithAll :: (HList outers -> oldInner -> SetupFunc newInner) -> TestDefM outers newInner result -> TestDefM outers oldInner result
setupAroundWithAll = setupAroundWith'
