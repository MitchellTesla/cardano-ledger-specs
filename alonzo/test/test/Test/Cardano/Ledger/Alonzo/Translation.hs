{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Translation
  ( tests,
  )
where

import Cardano.Binary
  ( ToCBOR (..),
  )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AuxiliaryData)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Translation (TxInBlock (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (TranslateEra (..))
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Shelley.Spec.Ledger.API as API
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.EraBuffet
  ( MaryEra,
    StandardCrypto,
  )
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

type Mary = MaryEra StandardCrypto

type Alonzo = AlonzoEra StandardCrypto

tests :: TestTree
tests =
  testGroup
    "Translation"
    [ alonzoTranslationTests,
      alonzoEncodeDecodeTests
    ]

alonzoEncodeDecodeTests :: TestTree
alonzoEncodeDecodeTests =
  testGroup
    "encoded mary types can be decoded as alonzo types"
    [ testProperty
        "decoding auxilliary"
        (decodeTestAnn @(MA.AuxiliaryData Mary) ([] :: [AuxiliaryData Alonzo])),
      testProperty
        "decoding txbody"
        (decodeTestAnn @(MA.TxBody Mary) ([] :: [TxBody Alonzo])),
      testProperty
        "decoding witnesses"
        (decodeTestAnn @(Core.Witnesses Mary) ([] :: [Core.Witnesses Alonzo]))
    ]

alonzoTranslationTests :: TestTree
alonzoTranslationTests =
  testGroup
    "Alonzo translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @API.Tx),
      testProperty "TxInBlock compatibility" (test @TxInBlock),
      testProperty "ProposedPPUpdates compatibility" (test @API.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @API.PPUPState),
      testProperty "UTxO compatibility" (test @API.UTxO),
      testProperty "UTxOState compatibility" (test @API.UTxOState),
      testProperty "LedgerState compatibility" (test @API.LedgerState)
    ]

deriving newtype instance
  (Arbitrary (Era.TxInBlock era)) =>
  Arbitrary (TxInBlock era)

dummyAlonzoGenesis :: AlonzoGenesis
dummyAlonzoGenesis = undefined

test ::
  forall f.
  ( ToCBOR (f Mary),
    ToCBOR (f Alonzo),
    TranslateEra Alonzo f,
    Show (TranslationError Alonzo f)
  ) =>
  f Mary ->
  Bool
test x = translationCompatToCBOR ([] :: [Alonzo]) dummyAlonzoGenesis x
