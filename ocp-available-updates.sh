#! /usr/bin/env bb
;; Fetches OpenShift upgrade paths
;; usage: ./ocp-available-updates.sh # asks for channel and version
;;  ./ocp-available-updates.sh channel # asks only for version
;;  ./ocp-available-updates.sh channel version # fixed stuff, no fzf

(require '[clojure.java.io :as io]
         '[clojure.java.shell :refer [sh]]
         '[cheshire.core :as json]
         )

(import 'java.lang.ProcessBuilder$Redirect)

(def channels ["stable-4.5" "stable-4.4" "stable-4.3"
               "fast-4.5" "fast-4.4" "fast-4.3"])

(defmacro with-filter
  [command & forms]
  `(let [sh#  (or (System/getenv "SHELL") "sh")
         pb#  (doto (ProcessBuilder. [sh# "-c" ~command])
                (.redirectError
                 (ProcessBuilder$Redirect/to (io/file "/dev/tty"))))
         p#   (.start pb#)
         in#  (io/reader (.getInputStream p#))
         out# (io/writer (.getOutputStream p#))]
     (binding [*out* out#]
       (try ~@forms (.close out#) (catch Exception e#)))
     (take-while identity (repeatedly #(.readLine in#)))))

(let [[channel version] *command-line-args*
      channel (or channel (first (with-filter "fzf"
                                   (dorun (map #(println %) channels)))))
      apiresponse (-> (sh "curl" "-H" "Accept:application/json"
                          (str "https://api.openshift.com/api/upgrades_info/v1/graph?channel=" channel))
                      :out
                      (json/parse-string true))
      version (or version (first (with-filter "fzf"
                                   (dorun (map println (map :version (:nodes apiresponse)))))))
      updates (as-> apiresponse $
                ((fn [m] (assoc m :versions (map :version (:nodes m)))
                   ) $)
                ((fn [m] (assoc m :vmap (map
                                         (fn [[a b]]
                                           [(nth (:versions m) a) (nth (:versions m) b)])
                                         (:edges m)))
                   ) $)
                (sort (map second (filter (fn [[a _]] (= version a)) (:vmap $))))
                )
      ]
  (println "Channel " channel)
  (println "Version:" version)
  (println "Available updates:" updates)
  )
