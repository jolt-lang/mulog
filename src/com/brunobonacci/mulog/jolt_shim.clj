(ns com.brunobonacci.mulog.jolt-shim
  "The three Java classes mulog bundles, as portable jolt registrations.

  Flake — the 192-bit time-ordered id — is ported bit-for-bit: getBytes,
  the homomorphic base64 string form, the hex form, parse, and UNSIGNED
  compareTo all match the JVM class, so flakes printed here read there and
  sort the same. ClojureThreadLocal rides on jolt.host/thread-id.
  ScheduledThreadPoolExecutor covers the surface mulog's timer pool reaches
  (scheduleAtFixedRate and the cancel/shutdown paths) over a future loop."
  (:require [clojure.string :as str]))

;; --- Flake -------------------------------------------------------------------
;; state: three signed 64-bit longs {:t :r1 :r2}.
(def ^:private flake-tag :mulog/flake)
(defn- tt [] (jolt.host/tagged-table flake-tag))
(defn- fget [f k] (jolt.host/ref-get f k))
(defn- mk-flake [t r1 r2]
  (doto (tt)
    (jolt.host/ref-put! :t t) (jolt.host/ref-put! :r1 r1) (jolt.host/ref-put! :r2 r2)))
(defn- flake? [x]
  (and (jolt.host/table? x) (= flake-tag (jolt.host/ref-get x :jolt/type))))

(def ^:private two64 18446744073709551616N)
(def ^:private two63 9223372036854775808N)
(defn- u64 [n] (mod n two64))
(defn- s64 [n] (let [m (mod n two64)] (if (>= m two63) (- m two64) m)))

;; monotonic epoch nanos (NanoClock.currentTimeNanos: wall-anchored, always
;; increasing in-process)
(def ^:private last-nanos (atom 0))
(defn- current-nanos []
  (swap! last-nanos (fn [prev] (max (inc prev) (* (System/currentTimeMillis) 1000000)))))

(def ^:private srng (delay (java.security.SecureRandom.)))
(defn- rand-long []
  (let [bs (byte-array 8)]
    (.nextBytes @srng bs)
    (s64 (reduce (fn [acc i] (+ (* acc 256) (bit-and (aget bs i) 0xFF))) 0 (range 8)))))

(defn- long->bytes [n]                    ; big-endian signed bytes
  (loop [i 7 v (u64 n) acc ()]
    (if (neg? i)
      acc
      (recur (dec i) (quot v 256)
             (cons (let [b (mod v 256)] (if (>= b 128) (- b 256) b)) acc)))))
(defn- flake-bytes [f]
  (byte-array (concat (long->bytes (fget f :t))
                      (long->bytes (fget f :r1))
                      (long->bytes (fget f :r2)))))

;; the homomorphic base64: unsigned byte sort order == string sort order
(def ^:private flake-chars "-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
(def ^:private flake-rev
  (into {} (map-indexed (fn [i c] [c i]) flake-chars)))
(defn- format-flake [f]
  (let [bs (flake-bytes f)
        ub (fn [i] (bit-and (aget bs i) 0xFF))]
    (apply str
      (mapcat (fn [i]
                (let [b1 (bit-shift-right (ub i) 2)
                      b2 (bit-or (bit-shift-left (bit-and (ub i) 2r11) 4)
                                 (bit-shift-right (ub (+ i 1)) 4))
                      b3 (bit-or (bit-shift-left (bit-and (ub (+ i 1)) 2r1111) 2)
                                 (bit-shift-right (ub (+ i 2)) 6))
                      b4 (bit-and (ub (+ i 2)) 2r111111)]
                  [(nth flake-chars b1) (nth flake-chars b2) (nth flake-chars b3) (nth flake-chars b4)]))
              (range 0 24 3)))))
(defn- format-flake-hex [f]
  (apply str (map (fn [b] (format "%02x" (bit-and b 0xFF))) (seq (flake-bytes f)))))
(defn- bytes->flake [bs]
  (when (and bs (= 24 (alength bs)))
    (let [rd (fn [off] (s64 (reduce (fn [acc i] (+ (* acc 256) (bit-and (aget bs (+ off i)) 0xFF))) 0 (range 8))))]
      (mk-flake (rd 0) (rd 8) (rd 16)))))
(defn- parse-flake [s]
  (when (and (string? s) (= 32 (count s)))
    (let [sixes (mapv flake-rev s)]
      (when (every? some? sixes)
        (bytes->flake
         (byte-array
          (mapcat (fn [i]
                    (let [a (sixes i) b (sixes (+ i 1)) c (sixes (+ i 2)) d (sixes (+ i 3))
                          x (bit-or (bit-shift-left a 2) (bit-shift-right b 4))
                          y (bit-or (bit-shift-left (bit-and b 2r1111) 4) (bit-shift-right c 2))
                          z (bit-or (bit-shift-left (bit-and c 2r11) 6) d)
                          sb (fn [v] (if (>= v 128) (- v 256) v))]
                      [(sb x) (sb y) (sb z)]))
                  (range 0 32 4))))))))
(defn- flake-cmp [a b]
  (if (nil? b)
    1
    (let [c (compare (u64 (fget a :t)) (u64 (fget b :t)))]
      (if-not (zero? c)
        c
        (let [c (compare (u64 (fget a :r1)) (u64 (fget b :r1)))]
          (if-not (zero? c) c (compare (u64 (fget a :r2)) (u64 (fget b :r2)))))))))

(doseq [nm ["Flake" "com.brunobonacci.mulog.core.Flake"]]
  (clojure.core/__register-class-statics! nm
    {"flake" (fn [] (mk-flake (current-nanos) (rand-long) (rand-long)))
     "makeFlake" (fn ([bs] (or (bytes->flake bs)
                               (throw (IllegalArgumentException. "Invalid flake length"))))
                     ([t r1 r2] (mk-flake t r1 r2)))
     "formatFlake" (fn [f] (format-flake f))
     "formatFlakeHex" (fn [f] (format-flake-hex f))
     "parseFlake" (fn [s] (parse-flake s))}))
(clojure.core/__register-class-methods! flake-tag
  {"getBytes" (fn [f] (flake-bytes f))
   "getTimestampNanos" (fn [f] (fget f :t))
   "getTimestampMicros" (fn [f] (quot (fget f :t) 1000))
   "getTimestampMillis" (fn [f] (quot (fget f :t) 1000000))
   "compareTo" (fn [f o] (flake-cmp f o))
   "toString" (fn [f] (format-flake f))})
(clojure.core/__register-instance-check!
  (fn [cn val] (if (and (or (= cn "com.brunobonacci.mulog.core.Flake") (= cn "Flake")) (flake? val)) true nil)))
(clojure.core/__register-class! flake?
                                (fn [_] "com.brunobonacci.mulog.core.Flake")
                                (fn [_] ["com.brunobonacci.mulog.core.Flake" "Comparable" "java.lang.Comparable"]))
(clojure.core/__register-eq! (fn [a b] (or (flake? a) (flake? b)))
                             (fn [a b] (and (flake? a) (flake? b) (zero? (flake-cmp a b)))))
(clojure.core/__register-hash! flake? (fn [f] (hash [(fget f :t) (fget f :r1) (fget f :r2)])))
(clojure.core/__register-compare! (fn [a b] (and (flake? a) (flake? b))) flake-cmp)
(clojure.core/__register-str! flake? format-flake)
(clojure.core/__register-pr! flake? (fn [f] (str "#mulog/flake \"" (format-flake f) "\"")))

;; --- ClojureThreadLocal ------------------------------------------------------
;; per-thread storage keyed by jolt.host/thread-id, as a reify so @tl reaches
;; the deref method; a dead thread's slot lives until the value does — fine for
;; the local-context use mulog puts it to.
(doseq [nm ["ClojureThreadLocal" "com.brunobonacci.mulog.core.ClojureThreadLocal"]]
  (clojure.core/__register-class-ctor! nm
    (fn [& [init]]
      (let [cells (atom {})
            current (fn [] (let [m @cells k (jolt.host/thread-id)]
                             (if (contains? m k) (get m k) init)))]
        (reify
          clojure.lang.IDeref
          (deref [_] (current))
          (get [_] (current))
          (set [_ v] (swap! cells assoc (jolt.host/thread-id) v) nil)
          (remove [_] (swap! cells dissoc (jolt.host/thread-id)) nil))))))

;; --- ScheduledThreadPoolExecutor --------------------------------------------
;; the surface mulog's timer pool reaches: scheduleAtFixedRate, and cancel /
;; shutdown. Each schedule runs a future loop with a stop flag.
(def ^:private stpe-tag :mulog/scheduled-pool)
(def ^:private sfut-tag :mulog/scheduled-future)
(doseq [nm ["ScheduledThreadPoolExecutor" "java.util.concurrent.ScheduledThreadPoolExecutor"]]
  (clojure.core/__register-class-ctor! nm
    (fn [& _]
      (doto (jolt.host/tagged-table stpe-tag)
        (jolt.host/ref-put! :tasks (atom []))
        (jolt.host/ref-put! :down (atom false))))))
(defn- ms-of [n unit]
  (long (.toMillis unit n)))
(clojure.core/__register-class-methods! stpe-tag
  {"scheduleAtFixedRate"
   (fn [self task delay period unit]
     (let [stop (atom false)
           down (jolt.host/ref-get self :down)
           dm (ms-of delay unit) pm (ms-of period unit)
           fut (future
                 (Thread/sleep dm)
                 (loop []
                   (when-not (or @stop @down)
                     (try (task) (catch Exception _ nil))
                     (Thread/sleep pm)
                     (recur))))
           sf (doto (jolt.host/tagged-table sfut-tag)
                (jolt.host/ref-put! :stop stop)
                (jolt.host/ref-put! :fut fut))]
       (swap! (jolt.host/ref-get self :tasks) conj sf)
       sf))
   "shutdown" (fn [self] (reset! (jolt.host/ref-get self :down) true) nil)
   "shutdownNow" (fn [self] (reset! (jolt.host/ref-get self :down) true) [])
   "setRemoveOnCancelPolicy" (fn [self _] nil)})
(clojure.core/__register-class-methods! sfut-tag
  {"cancel" (fn [self & _] (reset! (jolt.host/ref-get self :stop) true) true)
   "isCancelled" (fn [self] @(jolt.host/ref-get self :stop))
   "isDone" (fn [self] @(jolt.host/ref-get self :stop))})
