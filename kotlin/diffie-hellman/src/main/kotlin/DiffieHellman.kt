import java.math.BigInteger
import java.util.*

object DiffieHellman {
    fun privateKey(prime: BigInteger): BigInteger {
        val rand = Random()
        return generateSequence { BigInteger(prime.bitLength(), rand) }
                .filter { it > BigInteger.ONE && it < prime }
                .first()
    }

    fun publicKey(p: BigInteger, g: BigInteger, privKey: BigInteger): BigInteger
            = g.modPow(privKey, p)

    fun secret(prime: BigInteger, publicKey: BigInteger, privateKey: BigInteger): BigInteger
            = publicKey.modPow(privateKey, prime)
}
