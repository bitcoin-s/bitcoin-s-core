package org.bitcoins.testkitcore.gen

import org.bitcoins.core.config.Networks
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{DLCAccept, DLCOffer}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.util.sorted._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkitcore.dlc.DLCTestUtil
import org.bitcoins.testkitcore.gen.ln._
import org.scalacheck.Gen

trait TLVGen {

  def unknownTpe: Gen[BigSizeUInt] = {
    NumberGenerator.bigSizeUInt.suchThat(num => !TLV.knownTypes.contains(num))
  }

  def unknownTLV: Gen[UnknownTLV] = {
    for {
      tpe <- unknownTpe
      value <- NumberGenerator.bytevector
    } yield {
      UnknownTLV(tpe, value)
    }
  }

  def initTLV: Gen[InitTLV] = {
    for {
      globalFeatures <- NumberGenerator.bytevector
      features <- NumberGenerator.bytevector
      tlvNum <- Gen.choose(2, 10)
      // use unknown because some TLVs will make test really slow
      tlvs <- Gen.listOfN(tlvNum, unknownTLV.suchThat(_.tpe.toBigInt % 2 != 0))
    } yield {
      // get only one tlv per type
      val usedTLVs = tlvs.groupBy(_.tpe).map(_._2.head).toVector
      InitTLV(globalFeatures, features, usedTLVs)
    }
  }

  def errorTLV: Gen[ErrorTLV] = {
    for {
      id <- NumberGenerator.bytevector(32)
      data <- NumberGenerator.bytevector
    } yield {
      ErrorTLV(id, data)
    }
  }

  def amtToForwardTLV: Gen[AmtToForwardTLV] = {
    for {
      msat <- LnCurrencyUnitGen.milliSatoshis
    } yield {
      AmtToForwardTLV(msat)
    }
  }

  def outgoingCLTVValueTLV: Gen[OutgoingCLTVValueTLV] = {
    for {
      uint32 <- NumberGenerator.uInt32s
    } yield {
      OutgoingCLTVValueTLV(uint32)
    }
  }

  def shortChannelIdTLV: Gen[ShortChannelIdTLV] = {
    for {
      scid <- LnRouteGen.shortChannelId
    } yield {
      ShortChannelIdTLV(scid)
    }
  }

  def paymentDataTLV: Gen[PaymentDataTLV] = {
    for {
      secretTag <- LnInvoiceGen.secret
      msat <- LnCurrencyUnitGen.milliSatoshis
    } yield {
      PaymentDataTLV(secretTag.secret, msat)
    }
  }

  def pingTLV: Gen[PingTLV] = {
    for {
      num <- NumberGenerator.uInt16
      bytes <- NumberGenerator.bytevector
    } yield {
      PingTLV(num, bytes)
    }
  }

  def pongTLV: Gen[PongTLV] = {
    NumberGenerator.bytevector.map(PongTLV.forIgnored)
  }

  def enumEventDescriptorV0TLV: Gen[EnumEventDescriptorV0TLV] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genUTF8String)
    } yield EnumEventDescriptorV0TLV(outcomes.toVector)
  }

  def digitDecompositionEventDescriptorV0TLV: Gen[
    DigitDecompositionEventDescriptorV0TLV] = {
    for {
      base <- NumberGenerator.uInt16
      isSigned <- NumberGenerator.bool
      numDigits <- Gen.choose(2, 20)
      unit <- StringGenerators.genUTF8String
      precision <- NumberGenerator.int32s
    } yield DigitDecompositionEventDescriptorV0TLV(base,
                                                   isSigned,
                                                   numDigits,
                                                   unit,
                                                   precision)
  }

  def eventDescriptorTLV: Gen[EventDescriptorTLV] =
    Gen.oneOf(enumEventDescriptorV0TLV, digitDecompositionEventDescriptorV0TLV)

  def oracleEventV0TLV: Gen[OracleEventV0TLV] = {
    for {
      maturity <- NumberGenerator.uInt32s
      uri <- StringGenerators.genUTF8String
      desc <- eventDescriptorTLV
      nonces <-
        Gen
          .listOfN(desc.noncesNeeded, CryptoGenerators.schnorrNonce)
          .map(_.toVector)
    } yield OracleEventV0TLV(OrderedNonces.fromUnsorted(nonces).toVector,
                             maturity,
                             desc,
                             uri)
  }

  def oracleAnnouncementV0TLV: Gen[OracleAnnouncementV0TLV] = {
    for {
      sig <- CryptoGenerators.schnorrDigitalSignature
      pubkey <- CryptoGenerators.schnorrPublicKey
      eventTLV <- oracleEventV0TLV
    } yield OracleAnnouncementV0TLV(sig, pubkey, eventTLV)
  }

  def oracleAttestmentV0TLV: Gen[OracleAttestmentV0TLV] = {
    for {
      eventId <- StringGenerators.genUTF8String
      pubkey <- CryptoGenerators.schnorrPublicKey
      numSigs <- Gen.choose(1, 10)
      unsorted <-
        Gen
          .listOfN(numSigs, CryptoGenerators.schnorrDigitalSignature)
          .map(_.toVector)
      sigs = OrderedSchnorrSignatures.fromUnsorted(unsorted)
      outcomes <-
        Gen
          .listOfN(numSigs, StringGenerators.genUTF8String)
          .map(_.toVector)
    } yield OracleAttestmentV0TLV(eventId, pubkey, sigs.toVector, outcomes)
  }

  def contractDescriptorV0TLVWithTotalCollateral: Gen[
    (ContractDescriptorV0TLV, Satoshis)] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genString)
      totalInput <-
        Gen
          .choose(numOutcomes + 1L, Long.MaxValue / 10000L)
          .map(Satoshis.apply)
      (contractDescriptor, _) =
        DLCTestUtil.genContractDescriptors(outcomes.toVector, totalInput)
    } yield {
      (contractDescriptor.toTLV, totalInput)
    }
  }

  def contractDescriptorV0TLV: Gen[ContractDescriptorV0TLV] = {
    contractDescriptorV0TLVWithTotalCollateral.map(_._1)
  }

  def contractDescriptorV1TLVWithTotalCollateral: Gen[
    (ContractDescriptorV1TLV, Satoshis)] = {
    for {
      numDigits <- Gen.choose(3, 7)
      totalInput <-
        Gen
          .choose(numDigits + 1L, Long.MaxValue / 10000L)
          .map(Satoshis.apply)
      (contractDescriptor, _) =
        DLCTestUtil.genMultiDigitContractInfo(numDigits, totalInput)
    } yield {
      (contractDescriptor.toTLV, totalInput)
    }
  }

  def contractDescriptorV1TLV: Gen[ContractDescriptorV1TLV] = {
    contractDescriptorV1TLVWithTotalCollateral.map(_._1)
  }

  def contractDescriptorTLVWithTotalCollateral: Gen[
    (ContractDescriptorTLV, Satoshis)] = {
    Gen.oneOf(contractDescriptorV0TLVWithTotalCollateral,
              contractDescriptorV1TLVWithTotalCollateral)
  }

  def oracleInfoV0TLV(outcomes: Vector[String]): Gen[OracleInfoV0TLV] = {
    for {
      privKey <- CryptoGenerators.privateKey
      rValue <- CryptoGenerators.schnorrNonce
    } yield {
      OracleInfoV0TLV(
        OracleAnnouncementV0TLV.dummyForEventsAndKeys(
          privKey,
          rValue,
          outcomes.map(EnumOutcome.apply)))
    }
  }

  def oracleInfoV1TLV(outcomes: Vector[String]): Gen[OracleInfoV1TLV] = {
    for {
      numOracles <- Gen.choose(2, 10)
      threshold <- Gen.choose(1, numOracles)
      oracles <- Gen.listOfN(numOracles, oracleInfoV0TLV(outcomes))
    } yield {
      val announcements = oracles.map(_.announcement).toVector
      OracleInfoV1TLV(threshold, OrderedAnnouncements(announcements))
    }
  }

  def oracleInfoTLV(outcomes: Vector[String]): Gen[OracleInfoTLV] = {
    Gen.oneOf(oracleInfoV0TLV(outcomes), oracleInfoV1TLV(outcomes))
  }

  def oracleInfoV0TLV(numDigits: Int): Gen[OracleInfoV0TLV] = {
    for {
      privKey <- CryptoGenerators.privateKey
      unsorted <- Gen.listOfN(numDigits, CryptoGenerators.schnorrNonce)
      rValues = OrderedNonces.fromUnsorted(unsorted.toVector)
    } yield {
      OracleInfoV0TLV(OracleAnnouncementV0TLV.dummyForKeys(privKey, rValues))
    }
  }

  def oracleInfoV1TLV(numDigits: Int): Gen[OracleInfoV1TLV] = {
    for {
      numOracles <- Gen.choose(2, 10)
      threshold <- Gen.choose(1, numOracles)
      oracles <- Gen.listOfN(numOracles, oracleInfoV0TLV(numDigits))
    } yield {
      val announcements = oracles.map(_.announcement).toVector
      OracleInfoV1TLV(threshold, OrderedAnnouncements(announcements))
    }
  }

  def oracleInfoTLV(numDigits: Int): Gen[OracleInfoTLV] = {
    Gen.oneOf(oracleInfoV0TLV(numDigits), oracleInfoV1TLV(numDigits))
  }

  def oracleInfoV0TLV: Gen[OracleInfoV0TLV] = {
    for {
      isEnum <- NumberGenerator.bool
      oracleInfoV0TLV <-
        if (isEnum) {
          Gen
            .listOf(StringGenerators.genUTF8String)
            .flatMap(outcomes => oracleInfoV0TLV(outcomes.toVector))
        } else {
          Gen.choose(3, 7).flatMap(numDigits => oracleInfoV0TLV(numDigits))
        }
    } yield oracleInfoV0TLV
  }

  def oracleInfoV1TLV: Gen[OracleInfoV1TLV] = {
    for {
      isEnum <- NumberGenerator.bool
      oracleInfo <-
        if (isEnum) {
          Gen
            .listOf(StringGenerators.genUTF8String)
            .flatMap(outcomes => oracleInfoV1TLV(outcomes.toVector))
        } else {
          Gen.choose(3, 7).flatMap(numDigits => oracleInfoV1TLV(numDigits))
        }
    } yield oracleInfo
  }

  def oracleInfoTLV: Gen[OracleInfoTLV] = {
    Gen.oneOf(oracleInfoV0TLV, oracleInfoV1TLV)
  }

  def contractInfoV0TLV: Gen[ContractInfoV0TLV] = {
    for {
      (descriptor, totalCollateral) <-
        contractDescriptorTLVWithTotalCollateral
      oracleInfo <- descriptor match {
        case ContractDescriptorV0TLV(outcomeAndValues) =>
          val outcomes = outcomeAndValues.map(_._1)
          oracleInfoTLV(outcomes)
        case ContractDescriptorV1TLV(numDigits, _, _) =>
          oracleInfoTLV(numDigits)
      }
    } yield {
      ContractInfoV0TLV(totalCollateral, descriptor, oracleInfo)
    }
  }

  def oracleInfoV0TLVWithKeys: Gen[
    (OracleInfoV0TLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      privKey <- CryptoGenerators.privateKey
      kValue <- CryptoGenerators.privateKey
      outcomes <- Gen.listOf(StringGenerators.genUTF8String)
    } yield {
      (OracleInfoV0TLV(
         OracleAnnouncementV0TLV.dummyForEventsAndKeys(
           privKey,
           kValue.schnorrNonce,
           outcomes.toVector.map(EnumOutcome.apply))),
       privKey,
       kValue)
    }
  }

  def fundingInputP2WPKHTLV(ignoreSerialIds: Vector[UInt64] =
    Vector.empty): Gen[FundingInputV0TLV] = {
    for {
      prevTx <- TransactionGenerators.realisticTransactionWitnessOut
      prevTxVout <- Gen.choose(0, prevTx.outputs.length - 1)
      sequence <- NumberGenerator.uInt32s
      (spk, _) <- ScriptGenerators.p2wpkhSPKV0
      newOutput = prevTx.outputs(prevTxVout).copy(scriptPubKey = spk)
      newPrevTx = prevTx match {
        case transaction: NonWitnessTransaction =>
          BaseTransaction(transaction.version,
                          transaction.inputs,
                          transaction.outputs.updated(prevTxVout, newOutput),
                          transaction.lockTime)
        case wtx: WitnessTransaction =>
          wtx.copy(outputs = wtx.outputs.updated(prevTxVout, newOutput))
      }
    } yield {
      DLCFundingInputP2WPKHV0(DLCMessage.genSerialId(ignoreSerialIds),
                              newPrevTx,
                              UInt32(prevTxVout),
                              sequence).toTLV
    }
  }

  def fundingInputV0TLV(ignoreSerialIds: Vector[UInt64] = Vector.empty): Gen[
    FundingInputV0TLV] = {
    fundingInputP2WPKHTLV(ignoreSerialIds) // Soon to be Gen.oneOf
  }

  def fundingInputV0TLVs(
      collateralNeeded: CurrencyUnit,
      ignoreSerialIds: Vector[UInt64] = Vector.empty): Gen[
    Vector[FundingInputV0TLV]] = {
    for {
      numInputs <- Gen.choose(0, 5)
      inputs <- Gen.listOfN(numInputs, fundingInputV0TLV(ignoreSerialIds))
      input <- fundingInputV0TLV(ignoreSerialIds)
    } yield {
      val totalFunding =
        inputs.foldLeft[CurrencyUnit](Satoshis.zero)(_ + _.output.value)
      if (totalFunding <= collateralNeeded) {
        val output = input.prevTx.outputs(input.prevTxVout.toInt)
        val newOutput =
          output.copy(value = collateralNeeded - totalFunding + Bitcoins.one)
        val newOutputs =
          input.prevTx.outputs.updated(input.prevTxVout.toInt, newOutput)
        val newPrevTx = input.prevTx match {
          case tx: BaseTransaction =>
            tx.copy(outputs = newOutputs)
          case wtx: WitnessTransaction =>
            wtx.copy(outputs = newOutputs)
          case EmptyTransaction =>
            throw new RuntimeException(
              "FundingInputV0TLV generator malfunction")
        }
        inputs.toVector :+ input.copy(prevTx = newPrevTx)
      } else {
        inputs.toVector
      }
    }
  }

  def cetSignaturesV0TLV: Gen[CETSignaturesV0TLV] = {
    Gen
      .nonEmptyListOf(CryptoGenerators.adaptorSignature)
      .map(sigs => CETSignaturesV0TLV(sigs.toVector))
  }

  def cetSignaturesV0TLV(numCETs: Int): Gen[CETSignaturesV0TLV] = {
    Gen
      .listOfN(numCETs, CryptoGenerators.adaptorSignature)
      .map(sigs => CETSignaturesV0TLV(sigs.toVector))
  }

  def fundingSignaturesV0TLV: Gen[FundingSignaturesV0TLV] = {
    Gen.choose(1, 10).flatMap(fundingSignaturesV0TLV)
  }

  def fundingSignaturesV0TLV(numWitnesses: Int): Gen[FundingSignaturesV0TLV] = {
    Gen
      .listOfN(
        numWitnesses,
        WitnessGenerators.p2wpkhWitnessV0 // TODO: make more general
      )
      .map(witnesses => FundingSignaturesV0TLV(witnesses.toVector))
  }

  def dlcOfferTLV: Gen[DLCOfferTLV] = {
    for {
      chainHash <- Gen.oneOf(
        Networks.knownNetworks.map(_.chainParams.genesisBlock.blockHeader.hash))
      contractInfo <- contractInfoV0TLV
      fundingPubKey <- CryptoGenerators.publicKey
      payoutAddress <- AddressGenerator.bitcoinAddress
      payoutSerialId <- NumberGenerator.uInt64
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveRealistic
        .suchThat(_ <= contractInfo.totalCollateral)
      fundingInputs <- fundingInputV0TLVs(totalCollateralSatoshis)
      changeAddress <- AddressGenerator.bitcoinAddress
      changeSerialId <- NumberGenerator.uInt64
      fundOutputSerialId <- NumberGenerator.uInt64.suchThat(_ != changeSerialId)
      feeRate <- CurrencyUnitGenerator.positiveRealistic.map(
        SatoshisPerVirtualByte.apply)
      timeout1 <- NumberGenerator.uInt32s
      timeout2 <- NumberGenerator.uInt32s
    } yield {
      val (contractMaturityBound, contractTimeout) = if (timeout1 < timeout2) {
        (BlockTimeStamp(timeout1), BlockTimeStamp(timeout2))
      } else {
        (BlockTimeStamp(timeout2), BlockTimeStamp(timeout1))
      }

      DLCOfferTLV(
        protocolVersionOpt = None, //TODO: Comeback and change this
        contractFlags = 0.toByte,
        chainHash = chainHash,
        contractInfo = contractInfo,
        fundingPubKey = fundingPubKey,
        payoutSPK = payoutAddress.scriptPubKey,
        payoutSerialId = payoutSerialId,
        offererCollateralSatoshis = totalCollateralSatoshis,
        fundingInputs = fundingInputs,
        changeSPK = changeAddress.scriptPubKey,
        changeSerialId = changeSerialId,
        fundOutputSerialId = fundOutputSerialId,
        feeRate = feeRate,
        contractMaturityBound = contractMaturityBound,
        contractTimeout = contractTimeout
      )
    }
  }

  def dlcOfferTLVWithOracleKeys: Gen[
    (DLCOfferTLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      offer <- dlcOfferTLV
      (oracleInfo, oraclePrivKey, oracleRValue) <- oracleInfoV0TLVWithKeys
    } yield {
      (offer.copy(contractInfo = offer.contractInfo
         .asInstanceOf[ContractInfoV0TLV]
         .copy(oracleInfo = oracleInfo)),
       oraclePrivKey,
       oracleRValue)
    }
  }

  def dlcAcceptTLV: Gen[DLCAcceptTLV] = {
    for {
      tempContractId <- CryptoGenerators.sha256Digest
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveRealistic
      fundingPubKey <- CryptoGenerators.publicKey
      payoutAddress <- AddressGenerator.bitcoinAddress
      payoutSerialId <- NumberGenerator.uInt64
      fundingInputs <- fundingInputV0TLVs(totalCollateralSatoshis)
      changeAddress <- AddressGenerator.bitcoinAddress
      changeSerialId <- NumberGenerator.uInt64
      cetSigs <- cetSignaturesV0TLV
      refundSig <- CryptoGenerators.digitalSignature
    } yield {
      DLCAcceptTLV(
        tempContractId,
        totalCollateralSatoshis,
        fundingPubKey,
        payoutAddress.scriptPubKey,
        payoutSerialId,
        fundingInputs,
        changeAddress.scriptPubKey,
        changeSerialId,
        cetSigs,
        refundSig,
        NoNegotiationFieldsTLV
      )
    }
  }

  def dlcAcceptTLV(offer: DLCOfferTLV): Gen[DLCAcceptTLV] = {
    val contractInfo = ContractInfo.fromTLV(offer.contractInfo)

    for {
      fundingPubKey <- CryptoGenerators.publicKey
      payoutAddress <- AddressGenerator.bitcoinAddress
      payoutSerialId <- NumberGenerator.uInt64.suchThat(
        _ != offer.payoutSerialId)
      acceptCollateral =
        (contractInfo.totalCollateral - offer.offererCollateralSatoshis).satoshis.toLong
      fundingInputs <- fundingInputV0TLVs(
        Satoshis(acceptCollateral),
        offer.fundingInputs.map(_.inputSerialId))
      changeAddress <- AddressGenerator.bitcoinAddress
      changeSerialId <- NumberGenerator.uInt64.suchThat(num =>
        num != offer.changeSerialId && num != offer.fundOutputSerialId)
      cetSigs <- cetSignaturesV0TLV(contractInfo.allOutcomes.length)
      refundSig <- CryptoGenerators.digitalSignature
    } yield {
      DLCAcceptTLV(
        tempContractId = DLCOffer.fromTLV(offer).tempContractId,
        acceptCollateralSatoshis = Satoshis(acceptCollateral),
        fundingPubKey = fundingPubKey,
        payoutSPK = payoutAddress.scriptPubKey,
        payoutSerialId = payoutSerialId,
        fundingInputs = fundingInputs,
        changeSPK = changeAddress.scriptPubKey,
        changeSerialId = changeSerialId,
        cetSignatures = cetSigs,
        refundSignature = refundSig,
        negotiationFields = NoNegotiationFieldsTLV
      )
    }
  }

  def dlcOfferTLVAcceptTLV: Gen[(DLCOfferTLV, DLCAcceptTLV)] = {
    for {
      offer <- dlcOfferTLV
      accept <- dlcAcceptTLV(offer)
    } yield (offer, accept)
  }

  def dlcOfferTLVAcceptTLVWithOracleKeys: Gen[
    (DLCOfferTLV, DLCAcceptTLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      (offer, privKey, kVal) <- dlcOfferTLVWithOracleKeys
      accept <- dlcAcceptTLV(offer)
    } yield (offer, accept, privKey, kVal)
  }

  def dlcSignTLV: Gen[DLCSignTLV] = {
    for {
      contractId <- NumberGenerator.bytevector(32)
      cetSigs <- cetSignaturesV0TLV
      refundSig <- CryptoGenerators.digitalSignature
      fundingSigs <- fundingSignaturesV0TLV
    } yield {
      DLCSignTLV(contractId, cetSigs, refundSig, fundingSigs)
    }
  }

  def dlcSignTLV(offer: DLCOfferTLV, accept: DLCAcceptTLV): Gen[DLCSignTLV] = {
    val contractInfo = ContractInfo.fromTLV(offer.contractInfo)

    for {
      cetSigs <- cetSignaturesV0TLV(contractInfo.allOutcomes.length)
      refundSig <- CryptoGenerators.digitalSignature
      fundingSigs <- fundingSignaturesV0TLV(offer.fundingInputs.length)
    } yield {
      val deserOffer = DLCOffer.fromTLV(offer)
      val builder =
        DLCTxBuilder(deserOffer,
                     DLCAccept.fromTLV(accept, deserOffer).withoutSigs)
      val fundingTx = builder.buildFundingTx
      val contractId = fundingTx.txIdBE.bytes.xor(accept.tempContractId.bytes)

      DLCSignTLV(contractId, cetSigs, refundSig, fundingSigs)
    }
  }

  def dlcOfferTLVAcceptTLVSignTLV: Gen[
    (DLCOfferTLV, DLCAcceptTLV, DLCSignTLV)] = {
    for {
      (offer, accept) <- dlcOfferTLVAcceptTLV
      sign <- dlcSignTLV(offer, accept)
    } yield (offer, accept, sign)
  }

  def dlcOfferTLVAcceptTLVSignTLVWithOralceKeys: Gen[
    (DLCOfferTLV, DLCAcceptTLV, DLCSignTLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      (offer, accept, privKey, kValue) <- dlcOfferTLVAcceptTLVWithOracleKeys
      sign <- dlcSignTLV(offer, accept)
    } yield (offer, accept, sign, privKey, kValue)
  }

  def tlv: Gen[TLV] = {
    Gen.oneOf(
      unknownTLV,
      errorTLV,
      pingTLV,
      pongTLV,
      amtToForwardTLV,
      outgoingCLTVValueTLV,
      shortChannelIdTLV,
      paymentDataTLV,
      oracleEventV0TLV,
      eventDescriptorTLV,
      oracleAnnouncementV0TLV,
      contractInfoV0TLV,
      oracleInfoV0TLV,
      fundingInputV0TLV(),
      cetSignaturesV0TLV,
      fundingSignaturesV0TLV,
      dlcOfferTLV,
      dlcAcceptTLV,
      dlcSignTLV
    )
  }
}

object TLVGen extends TLVGen
