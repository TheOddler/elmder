module SydTestExtra where

import Test.Syd (HList, SetupFunc, TestDefM, setupAroundWith')

-- | This simply gives the compiler some more type information
setupAroundWithAll :: (HList outers -> oldInner -> SetupFunc newInner) -> TestDefM outers newInner result -> TestDefM outers oldInner result
setupAroundWithAll = setupAroundWith'
