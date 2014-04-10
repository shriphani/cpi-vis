(ns cpi-vis.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def *fertility-corpus* "rawdata_2127.txt")
(def *gdp-per-capita-corpus* "rawdata_2004.txt")

(defn fertility-corpus->csv
  []
  (with-open [wrtr (io/writer "fertility.csv")]
    (binding [*out* wrtr]
     (doseq [row
             (map
              (fn [l]
                (let [splitted (string/split l #"\s+")
                      value (-> splitted last Double/parseDouble)
                      country (string/join
                               " "
                               (drop-last
                                (rest splitted)))]
                  (str country "," value)))
              (string/split-lines
               (slurp *fertility-corpus*)))]
       (println row)))))

(defn gdp-per-capita-corpus->csv
  []
  (with-open [wrtr (io/writer "gdp.csv")]
    (binding [*out* wrtr]
     (doseq [row
             (map
              (fn [l]
                (let [splitted (string/split l #"\s+")
                      value (Double/parseDouble
                             (string/replace
                              (-> splitted last)
                              #","
                              ""))
                      country (string/join
                               " "
                               (drop-last
                                (drop-last
                                 (rest splitted))))]
                  (str country "," value)))
              (string/split-lines
               (slurp *gdp-per-capita-corpus*)))]
       (println row)))))

(defn combined-corpus
  []
  (let [gdp (into
             {}
             (map
              (fn [stuff]
                (let [c (drop-last stuff)
                      n (last stuff)]
                  [(string/trim
                    (string/join
                     " "
                     (reverse c))) n]))
              (map
               (fn [s]
                 (string/split s #","))
               (string/split-lines
                (slurp "gdp.csv")))))

        fertility (into
                   {}
                   (map
                    (fn [stuff]
                      (let [c (drop-last stuff)
                            n (last stuff)]
                        [(string/trim
                          (string/join
                           " "
                           (reverse c))) n]))
                    (map
                     (fn [s]
                       (string/split s #","))
                     (string/split-lines
                      (slurp "fertility.csv")))))]
    (with-open [wrtr (io/writer "data.csv")]
      (binding [*out* wrtr]
       (doseq
           [row
            (filter
             identity
             (map
              (fn [k]
                (when (and (fertility k)
                           (gdp k))
                  (str k "," (fertility k) "," (gdp k))))
              (keys fertility)))]
         (println row))))))
