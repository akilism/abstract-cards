(ns ab-can.core
  (:require
   #_[om.core :as om :include-macros true]
   [sablono.core :as sab :include-macros true])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]))

(enable-console-print!)

(defn draw-circle
  [[cx cy] r & opts]
  (let [opts (first opts)
        fill (:fill opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:circle {:cx cx
          :cy cy
          :r r
          :fill fill
          :stroke stroke
          :strokeWidth strokeWidth}]))

(defn draw-ellipse
  [[cx cy] rx ry & opts]
  (let [opts (first opts)
        fill (:fill opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:ellipse {:cx cx
          :cy cy
          :rx rx
          :ry ry
          :fill fill
          :stroke stroke
          :strokeWidth strokeWidth}]))

(defn draw-line
  [[[x1 y1] [x2 y2]] & opts]
  (let [opts (first opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:line {:x1 x1
            :y1 y1
            :x2 x2
            :y2 y2
            :stroke stroke
            :strokeWidth strokeWidth}]))

(defn draw-path
  [{:keys [d opts]}]
  (let [opts (first opts)
        fill (:fill opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:path {:d d
               :stroke stroke
               :strokeWidth strokeWidth
               :fill fill}]))

(defn draw-poly-line
  [{:keys [points opts]}]
  (let [opts (first opts)
        fill (:fill opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:polyline {:points points
               :stroke stroke
               :strokeWidth strokeWidth
               :fill fill}]))

(defn draw-poly
  [{:keys [points opts]}]
  (let [opts (first opts)
        fill (:fill opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:polygon {:points points
               :stroke stroke
               :strokeWidth strokeWidth
               :fill fill}]))

(defn draw-rect
  [[[x y] width height & opts]]
  (let [opts (first opts)
        fill (:fill opts)
        stroke (:stroke opts)
        strokeWidth (:strokeWidth opts)]
    [:rect {:x x
          :y y
          :width width
          :height height
          :fill fill
          :stroke stroke
          :strokeWidth strokeWidth}]))

(defn angle [n i]
  (/ (* 2 (.-PI js/Math) i) n))

(defn x-pos [x r n i]
  (+ x (* r (.sin js/Math (angle n i)))))

(defn y-pos [y r n i]
  (+ y (* r (.cos js/Math (angle n i )))))

(defn get-poly-points
  [sides side-length [x y] & opts]
  {:points (map (fn [i] [(x-pos x side-length sides i) (y-pos y side-length sides i)]) (range sides))
   :opts opts}
  ;[[60 20] [100 40] [100 80] [60 100] [20 80] [20 40]]
  )

(def hexagon (comp draw-poly (partial get-poly-points 6)))
(def triangle (comp draw-poly (partial get-poly-points 3)))
(def octagon (comp draw-poly (partial get-poly-points 8)))
(def decahedron (comp draw-poly (partial get-poly-points 12)))

(defn get-random-point
  [[sx sy] [ex ey] points i]
  (let [mx (- ex sx)
        my (- ey sy)]
    [(* (/ i points) mx (.random js/Math))
     (+ (* i points (.random js/Math)) my )]))

(defn random-quad
  [points control start end]
  (let [point (partial get-random-point start end points)]
    (map (fn [i]
          (cond
            (= 0 i) (str "M" (first start) " " (last start))
            (and (= 2 points) (= 1 i)) (str " q " (clojure.string/join " " control) ", " (clojure.string/join " " end))
            (and (< 2 points) (= 1 i)) (str " q " (clojure.string/join " " control) ", " (clojure.string/join " " (point i)))
            :else (str " t " (clojure.string/join " " (point i)))))
       (range points))))

(defn random-point
  [mx my]
  [(.round js/Math (* mx (.random js/Math)))
   (.round js/Math (* my (.random js/Math)))
   ;(+ (* 0.5 mx) (.round js/Math (* (* 1.75 mx) (.random js/Math))))
   ;(+ (* 0.5 my) (.round js/Math (* (* 0.5 my) (.random js/Math))))
   ])

(defn random-color
  []
  (let [r (.floor js/Math (* 255 (.random js/Math)))
        b (.floor js/Math (* 255 (.random js/Math)))
        g (.floor js/Math (* 255 (.random js/Math)))]
    (str "rgb(" r "," b "," g ")")))

(defn n-random-quads
  [count mx my]
  (map (fn [i]
         (let [control (random-point (* 0.5 mx) (* 0.5 my))
               start (random-point mx my)
               end (random-point mx my)
               points (* 15 (.random js/Math))
               stroke (random-color)
               strokeWidth (* 20 (.random js/Math))]
           (println start end control)
           (draw-path {:d (random-quad points control start end)
                       :opts [{:stroke stroke :strokeWidth strokeWidth :fill "transparent"}]}))) (range count)))

(defcard svg
  (sab/html [:div {:class "wrapper"}
             [:svg {:width "100%"
                    :height "100%"
                    :version "1.1"
                    :xmlns="http://www.w3.org/2000/svg"}
              ;(draw-line [[10 20] [100 20]] {:stroke "red" :strokeWidth 5})
              ;(draw-rect [[10 30] 100 30 {:fill "#fff" :stroke "#000" :strokeWidth 2}])
              ;(triangle 50 [50 50] {:fill "red" :stroke "#000" :strokeWidth 3})
              ;(draw-poly-line {:points [[10 10] [130 20] [10 30] [60 140]]
              ;                 :opts '({:stroke "blue" :strokeWidth 3 :fill "transparent"})})
              ;(hexagon 50 [250 250])
              ;(octagon 100 [150 150] {:fill "transparent" :strokeWidth 20 :stroke "black"})
              ;(octagon 50 [150 150])
              ;(draw-path {:d "M10 10 H 90 V 90 H 10 L 10 10"
              ;            :opts '({:stroke "black" :strokeWidth 2, :fill "transparent"})})
              ;(draw-path {:d "M130 60 C 110 80, 180 80, 170 60"
              ;            :opts '({:stroke "blue" :strokeWidth 2, :fill "transparent"})})
              ;(draw-path {:d "M10 80 C 40 10, 65 10, 95 80 S 150 150, 180 80"
              ;            :opts '({:stroke "green" :strokeWidth 2, :fill "transparent"})})
              ;(draw-path {:d "M10 80 Q 95 10 180 80"
              ;            :opts '({:stroke "red" :strokeWidth 2, :fill "transparent"})})
              ;(triangle 150 [150 200] {:fill "red"})

              ;(draw-circle [200 95] 80 {:fill "orange"})

              ;(draw-path {:d "M10 80 Q 52.5 10, 90 80 T 170 80 T 360 180 T 170 280"
              ;            :opts '({:stroke "purple" :strokeWidth 10, :fill "transparent"})})

              ;(draw-path {:d (random-quad 5 [65 250] [10 30] [360 10])
              ;            :opts '({:stroke "green" :strokeWidth 10, :fill "transparent"})})
              (n-random-quads (* 100 (.random js/Math)) 1500 1500)
              ]]))

(defcard svg2
  (sab/html [:div {:class "wrapper"}
             [:svg {:width "100%"
                    :height "100%"
                    :version "1.1"
                    :xmlns="http://www.w3.org/2000/svg"}
              (n-random-quads (* 100 (.random js/Math)) 1500 1500)]]))

(defcard svg3
  (sab/html [:div {:class "wrapper"}
             [:svg {:width "100%"
                    :height "100%"
                    :version "1.1"
                    :xmlns="http://www.w3.org/2000/svg"}
              (n-random-quads (* 100 (.random js/Math)) 2000 2000)]]))

(defcard svg4
  (sab/html [:div {:class "wrapper"}
             [:svg {:width "100%"
                    :height "100%"
                    :version "1.1"
                    :xmlns="http://www.w3.org/2000/svg"}
              (n-random-quads (* 100 (.random js/Math)) 100 50)]]))

(defcard svg5
  (sab/html [:div {:class "wrapper"}
             [:svg {:width "100%"
                    :height "100%"
                    :version "1.1"
                    :xmlns="http://www.w3.org/2000/svg"}
              (n-random-quads (* 100 (.random js/Math)) 150 150)]]))


(defn main []
  ;; conditionally start the app based on wether the #main-app-area
  ;; node is on the page
  (if-let [node (.getElementById js/document "main-app-area")]
    (js/React.render (sab/html [:div "This is working"]) node)))

(main)

;; remember to run lein figwheel and then browse to
;; http://localhost:3449/cards.html

