{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- 'w' is like in the writer example. Write log mesages of type w.
-- 's' specifies the endpoints
-- 'e' is the type of error messages
-- 'a' is the result of the computation
-- Contract w s e a
-- EmulatorTrace a

-- We dont want to write log messages () or endpoints (empty), error is Text, don't want a result
myContract1 :: Contract () Empty Text ()
myContract1 = do
    -- Throws the exception before the logging
    -- Comment the code below to see the logging
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

-- In order to test it
myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1
-- This can be tested using test1 previously loading the module
test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

-- The error type is Text->Void
-- () unit has one value called unit, void has no value. 
-- Contract can't produce or raise any exception
myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

-- Now we define custom endpoints:
-- 'foo' endpoint that takes an Int
-- .\/ type operator chains endpoints
-- 'bar' endpoint that takes a String
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

-- Pass the custom endpoint
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    -- Block execution until Int is provided and save it to 'n'
    n <- endpoint @"foo"
    Contract.logInfo n
    s <- endpoint @"bar"
    Contract.logInfo s

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    -- We can't have as before 'void $' we need the take the handle 'h<-'
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- Look at the first type parameter, the writer
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    -- 'void $' ignores the result of the wait
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
