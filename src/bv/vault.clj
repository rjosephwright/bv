(ns bv.vault
  (:require [amazonica.aws.kms :as kms]
            [bv.aws :refer [aws-seq to-either]]
            [cats.core :as m]
            [cats.monad.either :as e])
  (:import [javax.crypto Cipher KeyGenerator SecretKey]
           [javax.crypto.spec IvParameterSpec SecretKeySpec]
           [java.security SecureRandom]))

(defn fix-key-alias
  [a]
  (if (.startsWith a "alias/") a (str "alias/" a)))

(defn key-id-for-alias
  [ka list-aliases-response]
  (-> (filter #(= ka (:alias-name %)) list-aliases-response)
      first
      :target-key-id))

(defn find-or-create-master-key
  "Returns key id of given alias. If no key exists for alias, one will be created."
  [key-alias]
  (let [aliases (aws-seq kms/list-aliases :aliases)
        ka (fix-key-alias key-alias)
        key-id (key-id-for-alias ka aliases)]
    (if key-id
      key-id
      (let [res (kms/create-key)
            k (:key-id (:key-metadata res))]
        (kms/create-alias :alias-name ka :target-key-id k)
        k))))

(defn find-or-create-master-key-
  "Returns key id of given alias. If no key exists for alias, one will be created."
  [key-alias]
  (let [ka (fix-key-alias key-alias)]
    (m/mlet [aliases (to-either #(aws-seq kms/list-aliases :aliases))
             key-id (to-either #(key-id-for-alias ka aliases))]
      (if key-id
        (m/return key-id)
        (m/mlet [res (to-either #(kms/create-key))
                 :let [k (:key-id (:key-metadata res))]
                 _ (to-either #(kms/create-alias :alias-name ka :target-key-id k))]
          (m/return k))))))

(defn data-key
  [key-alias]
  (let [key-id (find-or-create-master-key key-alias)]
    (kms/generate-data-key :key-id key-id :key-spec "AES_256")))

(defn data-key-
  [key-alias]
  (m/mlet [key-id (find-or-create-master-key- key-alias)
           res (to-either #(kms/generate-data-key :key-id key-id :key-spec "AES_256"))]
    (m/return res)))

(defn as-bytes-
  [attr len data-key]
  (let [ba (byte-array len)]
    (m/mlet [d (to-either #(attr data-key))
             _ (to-either #(.get d ba))]
      (m/return ba))))

(defn plaintext-
  [data-key]
  (as-bytes- :plaintext 32 data-key))

(defn ciphertext-
  [data-key]
  (as-bytes- :ciphertext-blob 204 data-key))

(defn random-bytes
  [len]
  (let [sr (SecureRandom.)
        bytes (byte-array len)]
    (.nextBytes sr bytes)
    bytes))

(def blocksize 16)

(defn encrypt-payload
  [cleartext k]
  (let [sks (SecretKeySpec. k "AES")
        ivps (IvParameterSpec. (random-bytes blocksize))
        cipher (Cipher/getInstance "AES/CBC/PKCS5Padding")]
    (.init cipher (Cipher/ENCRYPT_MODE) sks ivps)
    (byte-array (mapcat seq [(.getIV ivps) (.doFinal cipher cleartext)]))))

(defn decrypt-payload
  [ciphertext k]
  (let [[iv payload] (map byte-array (split-at blocksize ciphertext))
        sks (SecretKeySpec. k "AES")
        ivps (IvParameterSpec. iv)
        cipher (Cipher/getInstance "AES/CBC/PKCS5Padding")]
    (.init cipher (Cipher/DECRYPT_MODE) sks ivps)
    (.doFinal cipher payload)))

(defn encrypt-payload-
  [cleartext k]
  (m/mlet [sks (to-either #(SecretKeySpec. k "AES"))
           ivps (to-either #(IvParameterSpec. (random-bytes blocksize)))
           cipher (to-either #(Cipher/getInstance "AES/CBC/PKCS5Padding"))
           _ (to-either #(.init cipher (Cipher/ENCRYPT_MODE) sks ivps))
           encrypted (to-either #(.doFinal cipher cleartext))]
    (m/return (byte-array (mapcat seq [(.getIV ivps) encrypted])))))

(defn decrypt-payload-
  [ciphertext k]
  (let [[iv payload] (map byte-array (split-at blocksize ciphertext))]
    (m/mlet [sks (to-either #(SecretKeySpec. k "AES"))
             ivps (to-either #(IvParameterSpec. iv))
             cipher (to-either #(Cipher/getInstance "AES/CBC/PKCS5Padding"))
             _ (to-either #(.init cipher (Cipher/DECRYPT_MODE) sks ivps))
             decrypted (to-either #(.doFinal cipher payload))]
      (m/return decrypted))))
