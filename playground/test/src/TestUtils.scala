package hermes.testing
import chisel3._
import chisel3.util.BitPat
import scala.language.implicitConversions

trait TestUtils {
  implicit def boolToBoolean(x : Bool) = x.litValue() == 1
  implicit def bitPatToUInt(b : BitPat) = BitPat.bitPatToUInt(b)
  implicit def uIntToBitPat(u : UInt) = BitPat(u)
  implicit def bigIntToInt(x : BigInt) = x != 0
  implicit def bigIntToBoolean(x : BigInt) = x != 0
  /**
    Chisel will throw error when x is negative number, this is a work around using >>>(zero-extend)
   */
  def toBigInt(x : Int): BigInt = (BigInt(x >>> 1) << 1) | (x & 0x1)
}