(ns lhrb.dumpsysinspector
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [oz.core :as oz]))


; --------------------------------------
; parsing
;
; extract frames from
; 'adb shell dumpsys gfxinfo pkg-name' command
; to produce the right output check in developer
; options
; -> Profile GPU rendering
; -> In adb shell dumpsys gfxinfo
; --------------------------------------

;"\tDraw\tPrepare\tProcess\tExecute"
(defn parse-frame
  [elem]
  (->> (str/split elem #"\t")
       (remove empty?)
       (map #(Float/valueOf %))))

(comment
  (parse-frame "\t4.90\t0.68\t4.29\t2.24"))

(defn parse-frames [dumpsys]
  (let [framebuilder (-> dumpsys
                         (str/split #"\n\n")
                         (nth 5))]
    (->> framebuilder
         str/split-lines
         (drop 2)
         (map parse-frame)
         (map #(zipmap [:draw :prepare :process :execute] %)))))

; --------------------------------------
; vega
;
; prepare data for displaying in a stacked bar chart
; --------------------------------------
(defn append-index [items]
  (map assoc items (repeat :index) (range)))

(defn shape-for-vega [m]
 (let [idx (select-keys m [:index])]
   (->>
    (dissoc m :index)
    (reduce-kv
     (fn [coll k v]
       (conj coll
             (merge idx
                    (assoc {}
                           :time v
                           :operation (name k)))))
     '()))))

(defn prepare-data [frames]
  (->> frames
       (append-index)
       (map shape-for-vega)
       flatten))

(defn stacked-bar [data]
  {:data {:values data}
   :width 800
   :mark {:type "bar"
          :binSpacing 0}
   :encoding {:x {:field "index"
                  :type "ordinal"
                  :title "frames"
                  :axis {:ticks false
                         :labels false}}
              :y {:aggregate "sum"
                  :field "time"
                  :type "quantitative"
                  :title "time in ms"}
              :color {:field "operation"
                      :type "nominal"}}})


(comment


 ; aggregate time
 #_(->> dumpsys
      parse-frames
      (map #(apply + (vals %)))
      (reduce +)))

; --------------------------------------
; IO
; --------------------------------------

(defn time-str []
  (let [formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd-hh:mm:ss")]
    (.format (java.time.LocalDateTime/now) formatter)))

(defn read-and-save [pkg-name filename]
  (->>
   (sh/sh "adb"
          "shell"
          "dumpsys"
          "gfxinfo"
          pkg-name)
   :out
   (spit filename)))

(defn files [times]
 (let [folder (str "resources/" (time-str) "/")]
   (.mkdirs (java.io.File. folder))
   (map (fn [n] (str folder n)) (range times))))

(defn record [pkg-name files]
  (loop [[head & tail] files]
    (Thread/sleep 2000)
    (read-and-save pkg-name head)
    (when-not (nil? tail)
      (recur tail))))

(defn create-chart [files]
  (->> files
       (map #(->> % slurp parse-frames))
       flatten
       (prepare-data)
       (stacked-bar)
       (oz/view!)))

(comment
  ; just some helper

 (oz/start-server!)

  (def pkg-name "pkg-name")

  ;; (files 10) => 20 sec recording
  (let [f (files 4)]
    (record pkg-name f)
    (create-chart f))

  (defn paths [path]
    (->>
     (java.io.File. path)
     file-seq
     (map #(.getPath %))
     sort))

  (paths "resources")

  (defn rel-paths
    "return only relevant file paths"
    [path]
    (->>
     (paths path)
     sort
     rest))

  (create-chart
   (rel-paths  "resources/2020-12-27-09:30:09"))
  )
