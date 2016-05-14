(ns bv.aws
  (:require [amazonica.core :as a]
            [cats.context :as c]
            [cats.core :as m]
            [cats.labs.state :as s]
            [cats.monad.either :as e]
            [clojure.string :refer [split]])
  (:import [com.amazonaws AmazonServiceException]))

(defn to-either
  "Convert action which may throw exception to Either.

  A Right value is returned on success, or a Left error,
  which is an exception that has been converted to a map."
  [action]
  (try
    (e/right (action))
    (catch AmazonServiceException ex
      (e/left (a/ex->map ex)))
    (catch Exception ex
      (e/left {:message (.getMessage ex)
               :error-type (last (split (str (.getClass ex)) #" "))
               :stack-trace (a/stack->string ex)}))))

(defn aws-seq
  "Provides a lazy sequence for AWS functions that return
  paginated results.

  For example, KMS `list-keys` or S3 `list-objects` functions return
  maps which may have the results truncated. When the results are
  truncated, a :next-marker key is provided whose value is an id
  which can be sent on a subsequent request to retrieve the next
  page of results.

  When calling this function, a lazy sequence will be returned which
  will be backed by multiple requests to AWS if needed to get all
  of the available results.

  To use this function, provide the AWS action (e.g. `list-aliases`),
  a map of options to the action, and the key within the returned map
  which returns the collection that is being sought (for `list-aliases`
  this is `:aliases`).

  This function can be called without the option map for actions that
  do not require such a map or already have sensible defaults.

  Examples:
  (aws-seq kms/list-aliases :aliases)

  (aws-seq s3/list-objects {:bucket-name \"rockafella\"} :object-summaries)"
  ([aws-action k]
   (aws-seq aws-action {} k))
  ([aws-action opts k]
   (lazy-seq
    (let [res (aws-action opts)
          coll (k res)
          ;; Sometimes the truncated key is called :truncated, other times :truncated?
          truncated? (if (nil? (get res :truncated?)) :truncated :truncated?)]
      (if (truncated? res)
        (let [newopts (assoc opts :marker (:next-marker res))]
          (concat coll (aws-seq aws-action newopts k)))
        coll)))))

(defn aws-sek
  ([action k]
   (aws-sek action {} k))
  ([action opts k]
   (to-either #(aws-seq action opts k))))

(defn aws-seq-
  ([aws-action k]
   (aws-seq- aws-action {} k))
  ([aws-action opts k]
   (m/mlet [res (to-either #(aws-seq aws-action opts k))
            :let [coll (lazy-seq (k res))
                  ;; Sometimes the truncated key is called
                  ;; :truncated?, other times :truncated
                  truncated? (if (nil? (get res :truncated?))
                               :truncated :truncated?)]]
     (if (truncated? res)
       (do
         (println "Gonna call AWS")
         (let [newopts (assoc opts :marker (:next-marker res))]
           (m/mlet [more (aws-seq- aws-action newopts k)]
             (m/return (lazy-seq (concat coll more))))))
       (m/return coll)))))

(defn aws-seq--
  ([aws-action k]
   (aws-seq-- aws-action {} k))
  ([aws-action opts k]
   (let [either-t (s/state-t e/context)]
     (c/with-context either-t
       (s/run-state
        (fn [s]
          (m/mlet [res (to-either #(aws-action opts))
                   :let [coll (lazy-seq (k res))
                         ;; Sometimes the truncated key is called
                         ;; :truncated?, other times :truncated
                         truncated? (if (nil? (get res :truncated?))
                                      :truncated :truncated?)]]
            (if (truncated? res)
              (do
                (println "Gonna call AWS")
                (let [newopts (assoc opts :marker (:next-marker res))]
                  (m/mlet [more (aws-seq-- aws-action newopts k)]
                    (m/return (lazy-seq (concat coll more))))))
              (m/return coll)))) {})))))
