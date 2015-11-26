;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assignment 4b - Graphic Drawing for Web Application
;; Darren Wiessner
;; CS696 - Functional Programming
;; 11/25/2015
;;
;;   The following application is a simple web based application that allows
;;   a user to draw lines, circles, and rectangles, as well as undo those
;;   actions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns graphic-drawing.core
  (:require [reagent.core :as r :refer [atom]]))

;; Just for testing. Remove prior to release
(enable-console-print!)

;; big ratom for holding application data
(defonce shape-data (r/atom {:first-x       "none"
                             :first-y       "none"
                             :second-x      "none"
                             :second-y      "none"
                             :shape         :none
                             :click-count   :first
                             :shape-list    '()}
                             :current-shape "none"))

;; Handles into the big ratom
(def shape         (r/cursor shape-data [:shape]))
(def first-x       (r/cursor shape-data [:first-x]))
(def first-y       (r/cursor shape-data [:first-y]))
(def second-x      (r/cursor shape-data [:second-x]))
(def second-y      (r/cursor shape-data [:second-y]))
(def click-count   (r/cursor shape-data [:click-count]))
(def shape-list    (r/cursor shape-data [:shape-list]))
(def current-shape (r/cursor shape-data [:current-shape]))

;; Some math helpers
(defn abs [n] (max n (- n)))
(defn sqr [n] (* n n))

;; handle to the shape list in function format
(defn my-shape-list []    (list @shape-list))
(defn my-current-shape [] (list @current-shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These functions handle interpretation of user inputs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-drawing-line!
  "Handles line drawing based on user input"
  [pos]
  (cond
    (= :first  @click-count)((reset! first-x (.-clientX pos))
                             (reset! first-y (- (.-clientY pos) 60))
                             (reset! click-count :second))
    (= :second @click-count)((reset! second-x (.-clientX pos))
                             (reset! second-y (- (.-clientY pos) 60))
                             (reset! click-count :first)
                             (swap!  shape-list conj
                                (vector :line {:x1 @first-x
                                               :y1 @first-y
                                               :x2 @second-x
                                               :y2 @second-y}))
                             (reset! current-shape "none"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-moving-line!
  "Handles line drawing based on user input"
  [pos]
  (when (= :second @click-count)
          ((reset! second-x (.-clientX pos))
           (reset! second-y (- (.-clientY pos) 60))
           (reset! current-shape
              (vector :line {:x1 @first-x
                             :y1 @first-y
                             :x2 @second-x
                             :y2 @second-y
                             :stroke "red"})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-drawing-circ!
  "Handles circle drawing based on user input"
  [pos]
  (cond
    (= :first @click-count)
        ((reset! first-x (.-clientX pos))
         (reset! first-y (- (.-clientY pos) 60))
         (reset! click-count :second))
    (= :second @click-count)
        ((reset! second-x (.-clientX pos))
         (reset! second-y (- (.-clientY pos) 60))
         (reset! click-count :first)
         (swap! shape-list conj
            (vector :circle {:cx @first-x
                             :cy @first-y
                             :r (Math/sqrt
                                  (+
                                    (sqr
                                      (- (max @first-x @second-x)
                                         (min @first-x @second-x)))
                                    (sqr
                                      (- (max @first-y @second-y)
                                         (min @first-y @second-y)))))
                              :fill "none"}))
         (reset! current-shape "none"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-moving-circ!
  "Handles circle moving based on user input"
  [pos]
  (when (= :second @click-count)
        ((reset! second-x (.-clientX pos))
         (reset! second-y (- (.-clientY pos) 60))
         (reset! current-shape
            (vector :circle {:cx @first-x
                             :cy @first-y
                             :r (Math/sqrt
                                  (+
                                    (sqr
                                      (- (max @first-x @second-x)
                                         (min @first-x @second-x)))
                                    (sqr
                                      (- (max @first-y @second-y)
                                         (min @first-y @second-y)))))
                              :fill "none"
                              :stroke "red"})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-drawing-rect!
  "Handles rectangle drawing based on user input"
  [pos]
  (cond
    (= :first @click-count)
       ((reset! first-x (.-clientX pos))
        (reset! first-y (- (.-clientY pos) 60))
        (reset! click-count :second))
    (= :second @click-count)
        ((reset! second-x (.-clientX pos))
         (reset! second-y (- (.-clientY pos) 60))
         (reset! click-count :first)
         (swap! shape-list conj
            (vector :rect {:x (min @first-x @second-x)
                           :y (min @first-y @second-y)
                           :width (abs (- @second-x @first-x))
                           :height (abs (- @second-y @first-y))
                           :fill "none"}))
         (reset! current-shape "none"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-moving-rect!
  "Handles rectangle moving based on user input"
  [pos]
  (when (= :second @click-count)
          ((reset! second-x (.-clientX pos))
           (reset! second-y (- (.-clientY pos) 60))
           (reset! current-shape
               (vector :rect {:x (min @first-x @second-x)
                              :y (min @first-y @second-y)
                              :width (abs (- @second-x @first-x))
                              :height (abs (- @second-y @first-y))
                              :fill "none"
                              :stroke "red"})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These are the functions that handle user interactions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-line-button-click!
  "This function handles user clicking the line button"
  []
  (reset! shape :line))


(defn handle-rect-button-click!
  "This function handles user clicking the rectangle button"
  []
  (reset! shape :rect))


(defn handle-circ-button-click!
  "This function handles user clicking the circle button"
  []
  (reset! shape :circ))


(defn handle-undo-button-click!
  "This function handles user clicking the undo button"
  []
  (cond
    (= :first  @click-count)
        (swap!  shape-list rest @shape-list)
    (= :second @click-count)
       ((reset! current-shape "none")
        (reset! click-count :first))))


(defn handle-palette-click!
  "This function handles user clicking on the palette"
  [pos]
  (cond
    (= :line @shape)(handle-drawing-line! pos)
    (= :circ @shape)(handle-drawing-circ! pos)
    (= :rect @shape)(handle-drawing-rect! pos)))


(defn handle-palette-move!
  "This function handles user moving the mouse on the palette"
  [pos]
  (cond
    (= :line @shape)(handle-moving-line! pos)
    (= :circ @shape)(handle-moving-circ! pos)
    (= :rect @shape)(handle-moving-rect! pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These are the main functions used to draw to the page
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn main []
  [:div
    [:div
      [:input#line-button {:type "button"
                           :on-click handle-line-button-click!
                           :value (str " LINE ")}]
      [:input#rect-button {:type "button"
                           :on-click handle-rect-button-click!
                           :value (str " RECTANGLE ")}]
      [:input#circ-button {:type "button"
                           :on-click handle-circ-button-click!
                           :value (str " CIRCLE ")}]
      [:input#undo-button {:type "button"
                           :on-click handle-undo-button-click!
                           :value (str " UNDO ")}]]
    [:div
      [:svg#palette {:width 700 :height 550 :stroke "black"
                    :on-click handle-palette-click!
                    :on-mouse-move handle-palette-move!}
    (my-shape-list)
    (my-current-shape)]]])

(r/render-component [main]
  (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
