package org.extism.sdk.support;

import java.security.MessageDigest;

public class Hashing {

    public static String sha256HexDigest(byte[] input) {
        try {
            var messageDigest = MessageDigest.getInstance("SHA-256");
            var messageDigestBytes = messageDigest.digest(input);

            var hexString = new StringBuilder();
            for (var b : messageDigestBytes) {
                hexString.append(String.format("%02x", b));
            }
            return hexString.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
