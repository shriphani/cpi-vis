(ns cpi-vis.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:use [incanter core charts stats datasets io]))

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
        (println "Country,Fertility,GDP")
        (doseq [row
                (filter
                 identity
                 (map
                  (fn [k]
                    (when (and (fertility k)
                               (gdp k))
                      (str k "," (fertility k) "," (gdp k))))
                  (keys fertility)))]
          (println row))))))

(def *developed-countries* (set
                            ["Andorra"
                             "Faroe Islands"
                             "Ireland"
                             "Monaco"
                             "Spain"
                             "Australia"
                             "Finland"
                             "Israel"
                             "Netherlands"
                             "Sweden"
                             "Austria"
                             "France"
                             "Italy"
                             "New Zealand"
                             "Switzerland"
                             "Belgium"
                             "Germany"
                             "Japan"
                             "Norway"
                             "Turkey"	
                             "Bermuda"
                             "Greece"
                             "Liechtenstein"
                             "Portugal"
                             "United Kingdom"
                             "Canada"
                             "Holy See"
                             "Luxembourg"
                             "San Marino"
                             "United States"
                             "Denmark"
                             "Iceland"
                             "Malta"
                             "South Africa"
                             "Cyprus"
                             "Czech Republic"
                             "Hong Kong"
                             "South Korea"
                             "Singapore"
                             "Slovak Republic"
                             "Slovenia"
                             "Taiwan"
                             "Guernsey"
                             "Jersey"]))

(def *asian-tiger-countries* (set ["Japan"
                                   "Taiwan"
                                   "Singapore"
                                   "South Korea"
                                   "Hong Kong"
                                   "Japan"]))

(defn augment-dataset
  []
  (with-open [wrtr (io/writer "augmented-data.csv")]
    (binding [*out* wrtr]
      (do
        (println "Country,Fertility,GDP,DEV")
        (doseq [l (rest
                   (string/split-lines
                    (slurp "data.csv")))]
          (let [[country fer gdp] (string/split l #",")

                line
                (if (some #{country} *developed-countries*)
                  (str country "," fer "," gdp ",Developed")
                  (str country "," fer "," gdp ",Not-Developed"))]
            (println line)))))))

(defn developed-dataset
  []
  (with-open [wrtr (io/writer "developed-data.csv")]
    (binding [*out* wrtr]
      (do
        (println "Country,Fertility,GDP")
        (doseq [l (rest
                   (string/split-lines
                    (slurp "data.csv")))]
          (let [[country fer gdp] (string/split l #",")]

            (when (some #{country} *developed-countries*)
              (println (str country "," fer "," gdp)))))))))

(defn asian-tiger-dataset
  []
  (with-open [wrtr (io/writer "asian-tiger-data.csv")]
    (binding [*out* wrtr]
      (do
        (println "Country,Fertility,GDP")
        (doseq [l (rest
                   (string/split-lines
                    (slurp "data.csv")))]
          (let [[country fer gdp] (string/split l #",")]

            (when (some #{country} *asian-tiger-countries*)
              (println (str country "," fer "," gdp)))))))))

(defn generate-full-plot
  [data-file]
  (let [data (read-dataset data-file :header true)

        plt
        (scatter-plot :Fertility
                      :GDP
                      :group-by :DEV
                      :data
                      data
                      :legend
                      true
                      :title
                      "GDP vs Fertility. Red circle represents developed countries")]
    (add-lines plt
               (map (fn [x] 2.1)
                    (rest (sel data :cols 2)))
               (rest (sel data :cols 2))
               :series-label "UN-Recommended Fertility Level")))

(defn generate-smaller-plot
  [data-file title]
  (let [data (read-dataset data-file :header true)

        plt
        (scatter-plot :Fertility
                      :GDP
                      :data data
                      :legend true
                      :title title)]
    (add-lines plt
               (map (fn [x] 2.1)
                    (rest (sel data :cols 2)))
               (rest (sel data :cols 2))
               :series-label "UN-Recommended Fertility Level")))

(defn save-plot
  []
  (do
   (save
    (generate-full-plot "augmented-data.csv")
    "gdp_vs_fertility.png"
    :width 900)
   (save
    (generate-smaller-plot
     "asian-tiger-data.csv"
     "GDP vs Fertility - Developed Asian Economies")
    "gdp_vs_fertility-asian-tiger.png"
    :width 900)
   (save
    (generate-smaller-plot
     "developed-data.csv"
     "GDP vs Fertility - Developed Economies")
    "gdp_vs_fertility-developed.png"
    :width 900)))
