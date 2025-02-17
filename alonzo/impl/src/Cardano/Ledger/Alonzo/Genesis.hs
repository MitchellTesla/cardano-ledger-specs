{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Alonzo.Genesis
  ( AlonzoGenesis (..),
    extendPPWithGenesis,
  )
where

import Cardano.Binary
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Coin
import Data.Coders
import Data.Functor.Identity
import Data.Map.Strict
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Numeric.Natural
import qualified Shelley.Spec.Ledger.PParams as Shelley

data AlonzoGenesis = AlonzoGenesis
  { adaPerUTxOWord :: Coin,
    costmdls :: Map Language CostModel,
    prices :: Prices,
    maxTxExUnits :: ExUnits,
    maxBlockExUnits :: ExUnits,
    maxValSize :: Natural,
    collateralPercentage :: Natural,
    maxCollateralInputs :: Natural
  }
  deriving (Eq, Generic, NoThunks)

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  Shelley.PParams' Identity era1 ->
  AlonzoGenesis ->
  PParams' Identity era2
extendPPWithGenesis
  pp
  AlonzoGenesis
    { adaPerUTxOWord,
      costmdls,
      prices,
      maxTxExUnits,
      maxBlockExUnits,
      maxValSize,
      collateralPercentage,
      maxCollateralInputs
    } =
    extendPP
      pp
      adaPerUTxOWord
      costmdls
      prices
      maxTxExUnits
      maxBlockExUnits
      maxValSize
      collateralPercentage
      maxCollateralInputs

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance FromCBOR AlonzoGenesis where
  fromCBOR =
    decode $
      RecD AlonzoGenesis
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance ToCBOR AlonzoGenesis where
  toCBOR
    AlonzoGenesis
      { adaPerUTxOWord,
        costmdls,
        prices,
        maxTxExUnits,
        maxBlockExUnits,
        maxValSize,
        collateralPercentage,
        maxCollateralInputs
      } =
      encode $
        Rec AlonzoGenesis
          !> To adaPerUTxOWord
          !> To costmdls
          !> To prices
          !> To maxTxExUnits
          !> To maxBlockExUnits
          !> To maxValSize
          !> To collateralPercentage
          !> To maxCollateralInputs
