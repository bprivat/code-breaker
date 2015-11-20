(ns code-breaker.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(enable-console-print!)

(def cells [:a :b :c
            :d :e :f
            :g :h :i])

(defn new-board
  []
  (into {} (map (fn [k v] [k v]) cells (shuffle (range 1 10)))))

(defn blank-board
  []
  (into {} (map (fn [k v] [k v]) cells (repeat 0))))

(defn gen-close-hit
  [board solution ks]
  (let [b-row (map #(board %) ks)
        s-row (map #(solution %) ks)
        s-set (set s-row)]
    (->> (map (fn [b s]
                (cond
                  (= b s) [0 1]
                  (contains? s-set b) [1 0]
                  :default [0 0])) b-row s-row)
         (reduce (fn [[total-close total-hit] [close hit]]
                   [(+ total-close close) (+ total-hit hit)]) [0 0]))))

(defn gen-close-hit-board
  [board solution]
  (let [[row-a-close row-a-hit] (gen-close-hit board solution [:a :b :c])
        [row-d-close row-d-hit] (gen-close-hit board solution [:d :e :f])
        [row-g-close row-g-hit] (gen-close-hit board solution [:g :h :i])
        [col-a-close col-a-hit] (gen-close-hit board solution [:a :d :g])
        [col-b-close col-b-hit] (gen-close-hit board solution [:b :e :h])
        [col-c-close col-c-hit] (gen-close-hit board solution [:c :f :i])]
    (assoc board :row-a-close row-a-close
                 :row-a-hit row-a-hit
                 :row-d-close row-d-close
                 :row-d-hit row-d-hit
                 :row-g-close row-g-close
                 :row-g-hit row-g-hit
                 :col-a-close col-a-close
                 :col-b-close col-b-close
                 :col-c-close col-c-close
                 :col-a-hit col-a-hit
                 :col-b-hit col-b-hit
                 :col-c-hit col-c-hit)))

(def init-data
  {:solution (new-board)
   :board (assoc (blank-board) :editable? true)
   :tries []})

;; --------------------------------------------------------------------------------
;; Parsing

(defmulti read om/dispatch)

(defmethod read :solution
  [{:keys [state] :as env} key params]
  {:value (:solution @state)})

(defmethod read :board
  [{:keys [state] :as env} key params]
  {:value (:board @state)})

(defmethod read :tries
  [{:keys [state] :as env} key params]
  {:value (:tries @state)})

(defmulti mutate om/dispatch)

(defmethod mutate 'cell/update
  [{:keys [state]} _ params]
  {:action
   (fn []
     (let [[cell n] (first params)]
       (swap! state update-in [:board] #(assoc % cell n))))})

(defmethod mutate 'board/submit
  [{:keys [state]} _ _]
  {:action
   (fn []
     (let [new-try (gen-close-hit-board (:board @state) (:solution @state))]
       (swap! state update-in [:tries] #(conj % (assoc new-try :editable? false)))))})

(defmethod mutate 'game/restart
  [{:keys [state]} _ _]
  {:action
   (fn []
     (reset! state {:solution (new-board)
                    :board (assoc (blank-board) :editable? true)
                    :tries []}))})

;; ------------------------------------------------------------------------------
;; Components

(defui EditCellView
  static om/IQuery
  (query [_]
   [])
  Object
  (render [this]
    (let [[k v :as cell] (om/props this)]
      (println "Render CellView" k v)
      (dom/div nil
        (dom/input #js {:type "text"
                        :value v
                        :maxlength "1"
                        :size "1"
                        :onChange #(om/transact! this `[(cell/update ~[k (int (.. % -target -value))])])
                        })))))
(def edit-cell-view (om/factory EditCellView))

(defui CellView
  Object
  (render [this]
    (let [[k v :as cell] (om/props this)]
      (println "Render CellView" k v)
      (dom/div nil
        (dom/span nil v)))))
(def cell-view (om/factory CellView))

(defmulti render-cell (fn [board key] (:editable? board)))

(defmethod render-cell true
  [board key]
  (edit-cell-view (find board key)))

(defmethod render-cell false
  [board key]
  (cell-view (find board key)))

(defui BoardView
  Object
  (render [this]
    (println "Render BoardView")
    (let [board (om/props this)]
      (dom/div nil
        (dom/table #js {:className "table table-bordered"
                        :style #js {:align "center"}}
          (dom/tbody nil
            (dom/tr nil
              (dom/td nil (render-cell board :a))
              (dom/td nil (render-cell board :b))
              (dom/td nil (render-cell board :c))
              (when (:row-a-close board)
                (dom/td nil (render-cell board :row-a-close)))
              (when (:row-a-hit board)
                (dom/td nil (render-cell board :row-a-hit))))
            (dom/tr nil
              (dom/td nil (render-cell board :d))
              (dom/td nil (render-cell board :e))
              (dom/td nil (render-cell board :f))
              (when (:row-d-close board)
                (dom/td nil (render-cell board :row-d-close)))
              (when (:row-d-hit board)
                (dom/td nil (render-cell board :row-d-hit))) )
            (dom/tr nil
              (dom/td nil (render-cell board :g))
              (dom/td nil (render-cell board :h))
              (dom/td nil (render-cell board :i))
              (when (:row-g-close board)
                (dom/td nil (render-cell board :row-g-close)))
              (when (:row-g-hit board)
                (dom/td nil (render-cell board :row-g-hit))))
            (when (:col-a-close board)
              (dom/tr nil
                (dom/td nil (render-cell board :col-a-close))
                (dom/td nil (render-cell board :col-b-close))
                (dom/td nil (render-cell board :col-c-close))))
            (when (:col-a-hit board)
              (dom/tr nil
                (dom/td nil (render-cell board :col-a-hit))
                (dom/td nil (render-cell board :col-b-hit))
                (dom/td nil (render-cell board :col-c-hit))))))))))

(def board-view (om/factory BoardView))

(defui RootView
  static om/IQuery
  (query [_]
    [:solution :board :tries])
  Object
  (render [this]
    (println "Render RootView")
    (let [{:keys [solution board tries] :as props} (om/props this)]
      (dom/div nil
        (dom/h2 nil "Code Breaker")
        (apply dom/div #js {:class "row"}
          (map #(dom/div #js {:className "col-md-2"} (board-view %)) tries))
        (dom/div nil (board-view board))
        (dom/button #js {:onClick #(om/transact! this '[(board/submit)])} "Submit")
        (dom/button #js {:onClick #(om/transact! this '[(game/restart)])} "Restart")))))

(def reconciler
  (om/reconciler
    {:state init-data
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler RootView (gdom/getElement "app"))
