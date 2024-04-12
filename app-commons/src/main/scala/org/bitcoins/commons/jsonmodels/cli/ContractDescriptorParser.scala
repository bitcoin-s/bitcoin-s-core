package org.bitcoins.commons.jsonmodels.cli

import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.protocol.dlc.models.DLCPayoutCurve
import org.bitcoins.core.protocol.tlv.{
  ContractDescriptorTLV,
  ContractDescriptorV0TLV,
  ContractDescriptorV1TLV,
  DLCSerializationVersion,
  DigitDecompositionEventDescriptorV0TLV,
  OracleAnnouncementTLV,
  RoundingIntervalsV0TLV,
  TLVPoint
}
import ujson.{Arr, Bool, Null, Num, Obj, Str}

object ContractDescriptorParser {

  def parseCmdLine(
      value: ujson.Value,
      announcementTLV: OracleAnnouncementTLV): ContractDescriptorTLV = {
    value match {
      case obj: Obj =>
        upickle.default
          .read[ContractDescriptorV0TLV](obj)(using Picklers.contractDescriptorV0)
      case arr: Arr =>
        //we read the number of digits from the announcement,
        //take in tlv points for the payout curve
        //and don't provide access to give a rounding mode as a parameter
        val payoutPoints: Vector[TLVPoint] = arr.value.toVector.map { pointJs =>
          upickle.default
            .read[TLVPoint](pointJs)(using Picklers.tlvPointReader)
        }

        val payoutCurve = DLCPayoutCurve
          .fromPoints(payoutPoints,
                      serializationVersion = DLCSerializationVersion.Beta)
          .toTLV
        val numDigits = announcementTLV.eventTLV.eventDescriptor
          .asInstanceOf[DigitDecompositionEventDescriptorV0TLV]
          .numDigits
          .toInt
        ContractDescriptorV1TLV(numDigits,
                                payoutCurve,
                                RoundingIntervalsV0TLV.noRounding)
      case fail @ (_: Num | _: Bool | Null | _: Str) =>
        sys.error(
          s"Cannot parse contract descriptor from $fail, expected json object or array")
    }
  }
}
