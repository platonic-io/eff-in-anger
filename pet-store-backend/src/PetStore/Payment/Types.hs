{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module PetStore.Payment.Types where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Char           (digitToInt)
import           GHC.Generics

data Payment = Payment { cardNumber :: String }
             deriving (Eq,Show,Generic,ToJSON,FromJSON)



checkCardNumber :: Payment -> Bool
checkCardNumber Payment{cardNumber} =
  computeLuhn cardNumber
  where
    doubleAndSum :: [Int] -> Int
    doubleAndSum = fst . foldr (\i (acc, isEven) -> (acc + nextStep isEven i, not isEven)) (0,False)
      where
        nextStep isEven i
          | isEven      = (uncurry (+) . (`divMod` 10) . (*2)) i
          | otherwise = i

    computeLuhn :: String -> Bool
    computeLuhn = (0 ==) . (`mod` 10) . doubleAndSum . fmap digitToInt


data PaymentResult = PaymentResult { _id     :: Integer
                                   , _result :: Bool
                                   }
                   | PaymentError { _reason :: String }
  deriving (Eq,Show)

instance ToJSON PaymentResult where
  toJSON (PaymentResult i r) = object [ "id" .= i , "paymentOk" .= r ]
  toJSON (PaymentError r)    = object [ "error" .= r ]

instance FromJSON PaymentResult where
  parseJSON = withObject "PaymentResult" $
              \ o ->
                (PaymentResult <$> o .: "id" <*> o .: "paymentOk")
                <|> (PaymentError <$> o .: "error")
