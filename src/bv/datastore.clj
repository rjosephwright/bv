(ns bv.datastore
  (:require [bv.aws :refer [to-either]]
            [cats.core :as m]
            [cats.monad.either :as e]
            [clojure.java.io :as io]
            [clojure.string :refer [join split]])
  (:import [java.io File]))

(defn descending-paths
  "Takes a pathname and returns a list of all of its parent directories
  in descending order:

  (descending-paths \"path/to/some/file\")
  ;; => (\"path\", \"path/to\", \"path/to/some\", \"path/to/some/file\")"
  [path]
  (let [parts (split path #"/")
        end (+ 1 (count parts))]
    (doall (map
            #(join "/" (first (split-at % parts)))
            (range 1 end)))))

(def base-dir "bossvault")

(defn file-path [parts] (join "/" parts))
  
(defn dir-path
  [account role artifact]
  (file-path [base-dir account role artifact]))

(defn data-path
  [account role artifact]
  (file-path [(dir-path account role artifact) "data"]))

(defn key-path
  [account role artifact]
  (file-path [(dir-path account role artifact) "key"]))

(defn exists?
  [account role artifact]
  (.exists (File. (dir-path account role artifact))))

(defn mkdir
  [dir]
  (m/mlet [_ (to-either #(io/make-parents dir))
           exists (to-either #(not (.mkdir (File. dir))))]
    (if exists
      (e/left {:error-type "StorageError"
               :message (str dir " already exists")
               :stack-trace nil})
      (e/right nil))))

(defn write-files
  [filespecs]
  (to-either #(doseq [[path data] filespecs]
                (with-open [f (io/output-stream path)]
                  (.write f data)))))

(defn store
  [account role artifact encrypted k]
  (let [dir (dir-path account role artifact)
        dp (data-path account role artifact)
        kp (key-path account role artifact)]
    (m/mlet [_ (mkdir dir)
             _ (write-files [[dp encrypted] [kp k]])]
      (m/return nil))))

(defn retrieve-payload
  [account role artifact]
  (to-either #(slurp (data-path account role artifact))))

(defn retrieve-key
  [account role artifact]
  (to-either #(slurp (key-path account role artifact))))
  
