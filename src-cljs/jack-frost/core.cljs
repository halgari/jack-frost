(ns jack-frost.core
  (:require [cljs.reader :refer [read-string]]))


(defn add-ptr [this v]
  (set! (.-ptr this) (+ v (.-ptr this))))

(defn get-byte [this]
  (let [d (.getUint8 this (.-ptr this))]
    (add-ptr this 1)
    d))

(defn get-int16 [this]
  (let [d (.getInt16 data ptr)]
    (set! (.-ptr this) (+ ptr 2))
    d))

(defn get-int32 [this]
  (let [d (.getInt32 this (.-ptr this))]
    (add-ptr this 4)
    d))

(defn get-float64 [this]
  (let [d (.getFloat64 data ptr)]
    (set! (.-ptr this) (+ ptr 8))
    d))

(defn set-byte [this itm]
  (let [d (.setUint8 this (.-ptr this) itm)]
    (add-ptr this 1)
    d))

(defn set-int16 [this itm]
  (let [d (.setInt16 data ptr itm)]
    (set! (.-ptr this) (+ ptr 2))
    d))

(defn set-int32 [this itm]
  (let [d (.setInt32 this (.-ptr this) itm)]
    (add-ptr this 4)
    d))

(defn set-float64 [this]
  (let [d (.setFloat64 data ptr itm)]
    (set! (.-ptr this) (+ ptr 8))
    d))



(def ^:const ^int ID_NIL 0)
(def ^:const ^int ID_TRUE 1)
(def ^:const ^int ID_FALSE 2)
(def ^:const ^int ID_NUMBER 3)
(def ^:const ^int ID_INT32 4)
(def ^:const ^int ID_MAP 5)
(def ^:const ^int ID_SET 6)
(def ^:const ^int ID_VECTOR 7)
(def ^:const ^int ID_STRING 8)
(def ^:const ^int ID_KEYWORD 9)
(def ^:const ^int ID_TERM 10) ;; Represents the end of a collection

(declare write-item)
(declare read-item)


(defn write-number [^DataOutputStream strm itm]
  (if (and (integer? itm)
           (< itm 2147483648)
           (> itm -2147483648))
    (do
      (set-byte strm ID_INT32)
      (set-int32 strm itm))
    (do
      (.write strm ID_NUMBER)
      (.writeDouble strm itm))))

(defn write-map [^DataOutputStream strm itm]
  (set-byte strm ID_MAP)
  (loop [s (seq itm)]
    (let [[k v] (first s)]
      (if s
        (do (write-item strm k)
            (write-item strm v)
            (recur (next s)))
        (.write strm ID_TERM)))))

(defn write-seq [^DataOutputStream strm itm]
  (set-byte strm ID_VECTOR)
  (loop [s (seq itm)]
    (if s
      (do (write-item strm (first s))
          (recur (next s)))
      (set-byte strm ID_TERM))))

(defn write-set [^DataOutputStream strm itm]
  (.write strm ID_SET)
  (loop [s (seq itm)]
    (if s
      (do (write-item strm (first s))
          (recur (next s)))
      (.write strm ID_TERM))))

(defn write-string [^DataOutputStream strm ^String itm]
  (let [bytes ^bytes (.getBytes itm charset)]
    (.write strm ID_STRING)
    (.writeShort strm (alength bytes))
    (.write strm bytes 0 (alength bytes))))

(defn write-keyword [^DataOutputStream strm ^String itm]
  (.write strm ID_KEYWORD)
  (write-item strm (namespace itm))
  (write-item strm (name itm)))

(defn write-item [^DataOutputStream strm itm]
  (cond
   (nil? itm) (.write strm ID_NIL)
   (true? itm) (set-byte strm ID_TRUE)
   (false? itm) (set-byte strm ID_FALSE)
   (number? itm) (write-number strm itm)
   (map?  itm) (write-map strm itm)
   (set? itm) (write-set strm itm)
   (string? itm) (write-string strm itm)
   (keyword? itm) (write-keyword strm itm)
   (or (seq? itm) (vector? itm)) (write-seq strm itm)
   :else (assert false (str "No dispatch for type " (type itm)))))

(defn read-vector [^DataInputStream strm id]
  (loop [acc (transient [])]
    (let [id (get-byte strm)]
      (if (= id ID_TERM)
        (persistent! acc)
        (recur (conj! acc (read-item strm id)))))))

(defn read-set [^DataInputStream strm id]
  (loop [acc (transient #{})]
    (let [id (.read strm)]
      (if (= id ID_TERM)
        (persistent! acc)
        (recur (conj! acc (read-item strm id)))))))

(defn read-map [^DataInputStream strm id]
  (loop [acc (transient {})]
    (let [id (.read strm)]
      (if (= id ID_TERM)
        (persistent! acc)
        (let [k (read-item strm id)
              v (read-item strm)]
          (recur (assoc! acc k v)))))))

(defn -read-string [^DataInputStream strm id]
  (let [len (.readShort strm)
        data (byte-array len)]
    (.read strm data 0 len)
    (String. data charset)))


(defn read-item
  ([^DataInputStream strm]
     (read-item strm (get-byte strm)))
  ([^DataInputStream strm id]
     (condp = id
       ID_NIL nil
       ID_TRUE true
       ID_FALSE false
       ID_NUMBER (.readDouble strm)
       ID_INT32 (get-int32 strm)
       ID_MAP (read-map strm id)
       ID_SET (read-set strm id)
       ID_STRING (-read-string strm id)
       ID_KEYWORD (keyword (read-item strm)
                           (read-item strm))
       ID_VECTOR (read-vector strm id))))


(defn freeze [itm]
  (let [array (js/ArrayBuffer. 100000)
        view (js/DataView. array)]
    (set! (.-ptr view) 0)
    (write-item view itm)
    array))

(defn thaw [array]
  (let [view (js/DataView. array)]
    (set! (.-ptr view) 0)
    (read-item view)))

(defn print-fn [x]
  (.log js/console (pr-str x)))

(set! *print-fn* print-fn)

(dotimes [x 10]
  (time (let [x (thaw (freeze (range 2000)))]
          #_(.log js/console x)))

  (time (let [x (read-string (pr-str (range 2000)))]
          #_(.log js/console x))))
