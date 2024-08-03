import java.math.BigInteger;
import java.util.Random;

class DiffieHellman {
    private Random random = new Random();

    BigInteger privateKey(BigInteger prime) {
        BigInteger key;
        do {
            key = new BigInteger(prime.bitLength(), random);
        } while (key.compareTo(BigInteger.ONE) < 1 || key.compareTo(prime) >= 0);
        return key;
    }

    BigInteger publicKey(BigInteger p, BigInteger g, BigInteger privateKey) {
        return g.modPow(privateKey, p);
    }

    BigInteger secret(BigInteger p, BigInteger publicKey, BigInteger privateKey) {
        return publicKey.modPow(privateKey, p);
    }
}
