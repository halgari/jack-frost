(ns jack-frost.core
  (:import [java.io DataInputStream DataOutputStream ByteArrayOutputStream ByteArrayInputStream]
           [java.nio.charset Charset]))

(set! *warn-on-reflection* true)

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

(def ^Charset charset (Charset/forName "UTF-8"))

(defn write-number [^DataOutputStream strm itm]
  (if (and (integer? itm)
           (< itm Integer/MAX_VALUE)
           (> itm Integer/MIN_VALUE))
    (do
      (.write strm ID_INT32)
      (.writeInt strm itm))
    (do
      (.write strm ID_NUMBER)
      (.writeDouble strm itm))))

(defn write-map [^DataOutputStream strm itm]
  (.write strm ID_MAP)
  (loop [s (seq itm)]
    (let [[k v] (first s)]
      (if s
        (do (write-item strm k)
            (write-item strm v)
            (recur (next s)))
        (.write strm ID_TERM)))))

(defn write-seq [^DataOutputStream strm itm]
  (.write strm ID_VECTOR)
  (loop [s (seq itm)]
    (if s
      (do (write-item strm (first s))
          (recur (next s)))
      (.write strm ID_TERM))))

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
   (instance? Boolean itm) (if itm
                             (.write strm ID_TRUE)
                             (.write strm ID_FALSE))
   (number? itm) (write-number strm itm)
   (map?  itm) (write-map strm itm)
   (set? itm) (write-set strm itm)
   (string? itm) (write-string strm itm)
   (keyword? itm) (write-keyword strm itm)
   (or (seq? itm) (vector? itm)) (write-seq strm itm)
   :else (assert false (str "No dispatch for type " (type itm)))))

(defn read-vector [^DataInputStream strm id]
  (loop [acc (transient [])]
    (let [id (.read strm)]
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
     (read-item strm (.read strm)))
  ([^DataInputStream strm id]
     (condp = id
       ID_NIL nil
       ID_TRUE true
       ID_FALSE false
       ID_NUMBER (.readDouble strm)
       ID_INT32 (.readInt strm)
       ID_MAP (read-map strm id)
       ID_SET (read-set strm id)
       ID_STRING (-read-string strm id)
       ID_KEYWORD (keyword (read-item strm)
                           (read-item strm))
       ID_VECTOR (read-vector strm id))))


(defn freeze [itm]
  (let [stream (ByteArrayOutputStream.)
        output (DataOutputStream. stream)]
    (write-item output itm)
    (.toByteArray stream)))

(defn thaw [arr]
  (let [stream (ByteArrayInputStream. arr)
        input (DataInputStream. stream)]
    (read-item input)))


