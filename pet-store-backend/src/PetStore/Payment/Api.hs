{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module PetStore.Payment.Api where

import           Network.HTTP.Client    (defaultManagerSettings, newManager)
import           PetStore.Payment.Types
import           Servant
import           Servant.Client

type PaymentApi = "payment"  :> ReqBody '[JSON] Payment :> Post '[JSON] PaymentResult

paymentApi :: Proxy PaymentApi
paymentApi = Proxy

-- * Payment Client

checkPayment :: Payment -> ClientM PaymentResult
checkPayment = client paymentApi

type PaymentClient = Payment -> IO PaymentResult

makeClient :: String -> Int -> IO PaymentClient
makeClient h p = do
  let baseUrl = BaseUrl Http h p ""
  env <- ClientEnv <$> newManager defaultManagerSettings <*> pure baseUrl <*> pure Nothing
  pure $ \ pay -> either (PaymentError . show) id <$> runClientM (checkPayment pay) env
