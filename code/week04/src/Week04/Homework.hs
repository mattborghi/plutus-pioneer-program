{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
-- import in order to use the wallets
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    -- void $ submitTx tx
    -- handle insufficient funds
    handleError (\err -> Contract.logError $ "caught: " ++ unpack err) $ void $ submitTx tx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h <- activateContractWallet (Wallet 1) payContract
    -- pay to wallet 2, so get the wallet hash
    let pkh = pubKeyHash $ walletPubKey $ Wallet 2
    callEndpoint @"pay" h $ PayParams
        {
            ppRecipient = pkh,
            ppLovelace = x
        }
    -- wait one slot
    void $ Emulator.waitNSlots 1
    -- xs <- observableState h
    -- Extras.logInfo $ show xs

    callEndpoint @"pay" h $ PayParams
        {
            ppRecipient = pkh,
            ppLovelace = y
        }
    -- wait one slot
    void $ Emulator.waitNSlots 1
    -- ys <- observableState h
    -- Extras.logInfo $ show ys

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
