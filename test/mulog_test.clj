(ns mulog-test
  "Flake bit-exactness against the JVM class (values measured on OpenJDK 20 /
  mulog 0.9.0) plus the log pipeline end to end."
  (:import [com.brunobonacci.mulog.core Flake])
  (:require [com.brunobonacci.mulog :as u]
            [com.brunobonacci.mulog.flakes :as flakes]))

(def failures (atom 0))
(defn check [label expected actual]
  (if (= expected actual)
    (println "  ok  " label)
    (do (swap! failures inc)
        (println "  FAIL" label "— expected" (pr-str expected) "got" (pr-str actual)))))

(defn -main [& _]
  (let [f (Flake/makeFlake 1755888000123456789 -1234567890123456789 42)]
    (check "string form matches the JVM class" "54sepJ1unGMirTwAVWOyuk---------e" (str f))
    (check "hex form matches" "185e2ad540bacd15eeddef0b82167eeb000000000000002a" (Flake/formatFlakeHex f))
    (check "parse round-trips" f (Flake/parseFlake (str f)))
    (check "print form is the tagged literal" "#mulog/flake \"54sepJ1unGMirTwAVWOyuk---------e\"" (pr-str f))
    (check "reader reads it back" f (read-string (pr-str f)))
    (check "unsigned ordering" [-1 -1 1]
           [(compare f (Flake/makeFlake 1755888000123456790 0 0))
            (.compareTo (Flake/makeFlake 1 0 0) (Flake/makeFlake 1 0 1))
            (compare (Flake/makeFlake -1 0 0) (Flake/makeFlake 1 0 0))]))
  (check "flakes are time-ordered" true
         (let [a (flakes/flake) b (flakes/flake)] (neg? (compare a b))))
  (let [captured (atom [])
        buf (com.brunobonacci.mulog.buffer/agent-buffer 100)
        stop (u/start-publisher! {:type :inline :publisher
                                  (reify com.brunobonacci.mulog.publisher.PPublisher
                                    (agent-buffer [_] buf)
                                    (publish-delay [_] 100)
                                    (publish [_ buffer]
                                      (swap! captured into (map second (com.brunobonacci.mulog.buffer/items buffer)))
                                      (com.brunobonacci.mulog.buffer/clear buffer)))})]
    (u/set-global-context! {:app "gate"})
    (u/with-context {:req 7} (u/log ::evt :x 1))
    (Thread/sleep 1000)
    (stop)
    (let [e (first (filter #(= :mulog-test/evt (:mulog/event-name %)) @captured))]
      (check "event published with merged context" ["gate" 7 1]
             [(:app e) (:req e) (:x e)])
      (check "trace-id is a flake" true (some? (:mulog/trace-id e)))))
  (if (pos? @failures)
    (throw (ex-info "failures" {:n @failures}))
    (println "all mulog checks passed")))
