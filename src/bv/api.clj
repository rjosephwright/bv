(ns bv.api
  (:require [bv.aws :refer [to-either]]
            [bv.datastore :as ds]
            [bv.vault :as v]
            [cats.core :as m]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [compojure.core :refer [defroutes ANY GET POST]])
            [liberator.core :refer [resource defresource]]
            [liberator.representation :refer [as-response ring-response]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [schema.core :as s]
  (:import [cats.monad.either Left Right]))

(s/defschema EncryptRequest
  {:account s/Str
   :role s/Str
   :artifact s/Str
   :contents s/})

(defresource root-resource
  :available-media-types ["text/plain"]
  :handle-ok "hey")

(defmacro if-e
  [binding on-error on-success]
  `(let [either# ~(binding 1)
         ~(binding 0) (deref either#)]
     (cond
       (instance? Left either#) ~on-error
       (instance? Right either#) ~on-success)))

(defresource key-resource
  [account role artifact]
  :allowed-methods [:get]
  :available-media-types ["application/octet-stream"]
  :exists? (fn [_]
             (if-e [res (ds/retrieve-key account role artifact)]
                   (let [path (join "/" [account role artifact])
                         msg (str path " not found")]
                     [false {:message {:error msg}
                             :representation {:media-type "application/json"}}])
                   [true {::data res}]))
  :handle-ok #(::data %))

(defn file->bytes
  [filespec]
  (let [b (byte-array (:size filespec))
        path (.getAbsolutePath (:tempfile filespec))]
    (with-open [f (io/input-stream path)]
      (.read f b))
       b))

(defn encrypt-and-store
  [account role artifact contents]
  (let [key-alias "bossvault"]
    (m/mlet [dk (v/data-key- key-alias)
             pt (v/plaintext- dk)
             ct (v/ciphertext- dk)
             b (to-either #(file->bytes contents))
             encrypted (v/encrypt-payload- b pt)
             _ (ds/store account role artifact encrypted ct)]
      (m/return nil))))

(defresource encrypt-resource
  [account role artifact contents]
  :allowed-methods [:post]
  :available-media-types ["application/json"]
  :exists? #(ds/exists? account role artifact)
  :post-to-existing? false
  :put-to-existing? false
  :post! (fn [ctx]
           (if-e [res (encrypt-and-store account role artifact contents)]
                 (do
                   (println res)
                   {:status 403
                    :body {:error "Could not store"}})
                 (do
                   (println res)
                   {:status 201
                    :body {:artifact artifact}}))))

(defresource encrypt-resource
  [account role artifact private-key]
  :post! #(do
            {:status 403
             :body {:error "Could not store"}}))

(defroutes app
  (ANY "/"
       []
       root-resource)
  (ANY "/key/:account/:role/artifacts"
       [account role artifact]
       (key-resource account role artifact))
  (ANY "/encrypt/:account/:role/artifacts"
       [account role]
       {params :params}
       (let [{artifact "artifact" contents "contents"} params]
         (encrypt-resource account role artifact contents)))
  (ANY "/decrypt/:account/:role/artifacts"
       [account role]
       {params :params}
       (let [{artifact "artifact" private-key "privateKey"} params]
         (decrypt-resource account role artifact private-key))))

(def handler
  (-> app
      wrap-params
      wrap-multipart-params))
