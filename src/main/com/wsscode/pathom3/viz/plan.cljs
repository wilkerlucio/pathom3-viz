(ns com.wsscode.pathom3.viz.plan
  (:require [applied-science.js-interop :as j]
            ["dagre" :as dagre]
            ["cytoscape" :as cytoscape]
            ["cytoscape-dagre" :as cytoscape-dagre]
            [cljs.tools.reader :refer [read-string]]
            [com.wsscode.pathom3.connect.planner :as pcp]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.entity-tree :as p.ent]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [com.wsscode.js.browser-local-storage :as ls]
            [edn-query-language.core :as eql]
            [com.wsscode.pathom3.viz.ui :as ui]
            [goog.object :as gobj]
            [com.wsscode.js-interop.js-proxy :refer [jsp]]
            [helix.core :as h :refer [$]]
            [helix.dom :as dom]
            [helix.hooks :as hooks]))

(.use cytoscape cytoscape-dagre)

(pco/defresolver node-type [env _]
  {::pco/input [::pcp/node-id]}
  {::pcp/node-type (pcp/node-kind (p.ent/entity env))})

(pco/defresolver node-label [env {::pcp/keys [node-type]}]
  {::node-label
   (case node-type
     ::pcp/node-and "AND"
     ::pcp/node-or "OR"
     ::pcp/node-resolver (::pco/op-name (p.ent/entity env)))})

(pco/defresolver node-color [{::pcp/keys [node-type]}]
  {::node-color
   (case node-type
     ::pcp/node-and "yellow"
     ::pcp/node-or "cyan"
     ::pcp/node-resolver "gray")})

(def node-extensions-registry [node-type node-label node-color])

(def node-extensions-env
  (-> (pci/register node-extensions-registry)
      (psm/with-keys-mode ::psm/keys-mode-reachable)))

(defn new-graph []
  (new (.. dagre -graphlib -Graph)))

(defn layout-graph [{::pcp/keys [nodes root]}]
  (let [nodes' (vals nodes)
        g      (doto (new-graph)
                 (.setGraph #js {:rankdir "LR"
                                 :marginx 10
                                 :marginy 10})
                 (.setDefaultEdgeLabel (fn [] #js {})))]

    (doseq [{::pcp/keys [node-id run-next]
             ::keys     [node-label node-color]
             :as        node} nodes']
      (.setNode g node-id #js {:id     node-id
                               :label  node-label
                               :color  node-color
                               :root   (= root node-id)
                               :width  30
                               :height 30})

      (if run-next
        (.setEdge g node-id run-next #js {:runNext true}))

      (doseq [branch (pcp/node-branches node)]
        (.setEdge g node-id branch)))

    (.layout dagre g)
    g))

(defn frames [{::pci/keys [index-oir]
               ::pcp/keys [available-data]
               ::eql/keys [query]}]
  (let [snapshots* (atom [])
        graph      (pcp/compute-run-graph
                     (cond-> {::pci/index-oir              index-oir
                              ::pcp/snapshots*             snapshots*
                              :edn-query-language.ast/node (eql/query->ast query)}
                       available-data
                       (assoc ::pcp/available-data available-data)))
        frames     (-> (mapv #(psm/smart-map node-extensions-env %) @snapshots*)
                       (conj (psm/smart-map node-extensions-env (assoc graph ::pcp/snapshot-message "Completed graph."))))]
    frames))

(defn pos->coord [{::keys [x y]}]
  (str x "," y))

(defn create-path-line
  [pos-a pos-b]
  (str "M " (pos->coord pos-a) " L " (pos->coord pos-b)))

(defn create-path-curve
  [{xa ::x ya ::y :as pos-a} {xb ::x yb ::y :as pos-b}]
  (let [center     (+ ya (/ (- yb ya) 2))
        smoothness (/ (- xb xa) 8)]
    (str "M " (pos->coord pos-a) " C "
      (+ xa smoothness) "," center " "
      (- xb smoothness) "," center " "
      (pos->coord pos-b))))

(defn use-persistent-state [store-key initial-value]
  (let [[value set-value!] (hooks/use-state (ls/get store-key initial-value))
        set-persistent! (fn [x] (ls/set! store-key x) (doto x set-value!))]
    [value set-persistent!]))

(h/defnc PlanGraph [{:keys [data display-type]}]
  (dom/svg {:width 500 :height 500}
    (for [item (mapv #(.node data %) (.nodes data))
          :let [{:keys [id root label color width height x y]} (j/lookup item)]]
      (h/<> {:key (str "item" id)}
        (dom/circle {:key   (str "item-circle-" id)
                     :style (cond-> {:fill color}
                              root
                              (assoc :stroke "blue" :strokeWidth "2px"))
                     :r     (/ width 2)
                     :cx    x
                     :cy    y})
        (dom/text {:key              (str "item-text-" id)
                   :textAnchor       "middle"
                   :dominantBaseline "middle"
                   :x                x
                   :y                y}
          (str
            (case display-type
              ::display-type-node-id
              id

              ::display-type-label
              label

              "Invalid display type")))))

    (for [item (mapv #(.edge data %) (.edges data))
          :let [{:keys [points runNext]} (j/lookup item)
                p0 (j/lookup (first points))
                p1 (j/lookup (last points))]]
      (dom/path {:key   (str "path-" (js/JSON.stringify item))
                 :style {:fill   "none"
                         :stroke (if runNext "#000" "#ff9517")}
                 :d     (create-path-curve
                          {::x (:x p0) ::y (:y p0)}
                          {::x (:x p1) ::y (:y p1)})}))))

(h/defnc PlanControls [{:keys [frames-count frame-state display-type-state]}]
  (let [[current-frame set-current-frame!] frame-state]
    (dom/div {:style {:display "flex" :align-items "center"}}
      (dom/input
        {:type "range"
         :min  0
         :max  (dec frames-count)
         :step 1
         :&    (ui/dom-props {::ui/state [current-frame #(set-current-frame! (js/parseInt %))]})})
      (ui/dom-select {::ui/options [[::display-type-node-id "Node ID"]
                                    [::display-type-label "Label"]]
                      ::ui/state   display-type-state}))))

(h/defnc PlanView [{:keys [frames]}]
  (let [[current-frame :as frame-state] (hooks/use-state (dec (count frames)))
        [display-type :as display-type-state] (use-persistent-state ::display-type ::display-type-node-id)
        [{::pcp/keys [snapshot-event snapshot-message] :as graph} data] (get frames current-frame)]
    (dom/div
      ($ PlanControls {:frame-state        frame-state
                       :frames-count       (count frames)
                       :display-type-state display-type-state})
      (dom/div (or snapshot-message (pr-str snapshot-event)))
      ($ PlanGraph {:data data :display-type display-type})
      (dom/div {:style {:display "flex"}}
        (dom/div {:style {:marginRight "2rem"}}
          (for [[attr node-id] (sort-by first (::pcp/index-attrs graph))]
            (dom/div {:key node-id} (pr-str attr) " - " (str node-id))))
        (dom/div
          (for [[resolver node-id] (sort-by first (::pcp/index-resolver->nodes graph))]
            (dom/div {:key node-id} (pr-str resolver) " - " (str node-id))))))))

;;;;;;;;;; cytoscape version

(defn c-nodes-edges [{::pcp/keys [nodes root]}]
  (let [nodes'  (vals nodes)
        c-nodes (mapv
                  (fn [{::pcp/keys [node-id]
                        ::keys     [node-label node-color]}]
                    {:group   "nodes"
                     :data    {:id    (str node-id)
                               :label node-label
                               :color node-color}
                     :classes (cond-> [] (= root node-id) (conj "root"))})
                  nodes')
        all     (into c-nodes
                      (mapcat
                        (fn [{::pcp/keys [node-id run-next]
                              :as        node}]
                          (cond->
                            (for [branch (pcp/node-branches node)]
                              {:group   "edges"
                               :data    {:id (str node-id "," branch ",B") :source node-id :target branch}
                               :classes ["branch"]})

                            run-next
                            (conj {:group   "edges"
                                   :data    {:id (str node-id "," run-next ",N") :source node-id :target run-next}
                                   :classes ["next"]}))))
                      nodes')]
    all))

(defn create-coll [^js cy elements]
  (.add (.collection cy) (into-array elements)))

(defn node-diff [^js cy new-els current-els]
  (let [current-els-ids   (into #{} (map #(.id %)) current-els)
        new-state-els-ids (into #{} (map #(-> % :data :id)) new-els)
        add-els           (remove #(contains? current-els-ids (-> % :data :id)) new-els)
        remove-els        (remove #(contains? new-state-els-ids (.id %)) current-els)]
    [add-els (create-coll cy remove-els)]))

(defn display-type->label [display-type]
  (case display-type
    ::display-type-node-id
    "data(id)"

    ::display-type-label
    "data(label)"

    "X"))

(def anim-duration 300)

(defn add-fade-in [^js cy elms]
  (-> (.add cy elms)
      (.style "opacity" 0)
      (.animate #js {:style    #js {:opacity 1}
                     :duration anim-duration
                     :easing   "ease-in-sine"})))

(defn remove-fade-out [^js cy coll]
  (-> coll
      (.animate #js {:style    #js {:opacity 0}
                     :duration anim-duration
                     :easing   "ease-in-sine"
                     :complete #(.remove cy coll)})))

(defn cytoscape-planner-effect
  [{:keys [container-ref display-type]} elements]
  (let [cy-ref (hooks/use-ref nil)]
    (hooks/use-effect [display-type]
      (some-> @cy-ref
              (.style)
              (.selector "node")
              (.style "label" (display-type->label display-type))
              (.update)))
    (hooks/use-effect [elements]
      (if @cy-ref
        (let [cy         @cy-ref
              {:strs [nodes edges]} (group-by :group elements)
              [add-nodes remove-nodes] (node-diff cy nodes (.nodes cy))
              [add-edges remove-edges] (node-diff cy edges (.edges cy))
              remove-all (.add remove-nodes remove-edges)]
          (.batch cy
            (fn []
              (remove-fade-out cy remove-all)
              (doseq [{:keys [data classes]} nodes]
                (when-let [node (first (.nodes cy (str "[id=\"" (:id data) "\"]")))]
                  (.classes node (into-array classes))))
              (add-fade-in cy (clj->js add-nodes))
              (add-fade-in cy (clj->js add-edges))))
          (-> cy
              (.elements)
              (.difference remove-all)
              (.layout #js {:name "dagre" :rankDir "LR" :animate true :animationDuration anim-duration})
              (.run)))
        (reset! cy-ref
          (cytoscape
            #js {:container     @container-ref
                 :autoungrabify true
                 :layout        #js {:name    "dagre"
                                     :rankDir "LR"}
                 :style         #js [#js {:selector "node"
                                          :style    #js {:border-width        3
                                                         :shape               "round-rectangle"
                                                         :border-color        "data(color)"
                                                         :label               (display-type->label display-type)
                                                         :text-valign         "center"
                                                         :background-color    "data(color)"
                                                         :transition-property "border-color"
                                                         :transition-duration (str anim-duration "ms")}}
                                     #js {:selector "node.root"
                                          :style    #js {:border-width 3
                                                         :border-color "#00c"}}
                                     #js {:selector "edge"
                                          :style    #js {:curve-style        "bezier"
                                                         :width              2
                                                         :arrow-scale        0.8
                                                         :target-arrow-shape "triangle"}}
                                     #js {:selector "edge.branch"
                                          :style    #js {:line-color         "#ff9517"
                                                         :target-arrow-color "#ff9517"}}
                                     #js {:selector "edge.next"
                                          :style    #js {:line-color         "#000"
                                                         :target-arrow-color "#000"}}]
                 :elements      (clj->js elements)}))))))

(h/defnc PlanCytoscape [{:keys [frames]}]
  (let [[current-frame :as frame-state] (hooks/use-state (dec (count frames)))
        [{::pcp/keys [snapshot-event snapshot-message] :as graph} elements] (get frames current-frame)
        [display-type :as display-type-state] (use-persistent-state ::display-type ::display-type-node-id)
        container-ref (hooks/use-ref nil)]
    (cytoscape-planner-effect {:container-ref container-ref
                               :display-type  display-type} elements)
    (dom/div {:style {:width          "100%"
                      :display        "flex"
                      :flex-direction "column"}}
      ($ PlanControls {:frame-state        frame-state
                       :frames-count       (count frames)
                       :display-type-state display-type-state})
      (dom/div (or snapshot-message (pr-str snapshot-event)))
      (dom/div {:style {:flex "1"}
                :ref   #(reset! container-ref %)}))))

(comment
  (gobj/equals #js {:foo #js ["bar"]} #js {:foo #js ["bar"]})

  (cytoscape #js {:elements (clj->js)})

  (-> (frames '{::pci/index-oir {:a {#{:c :d} #{a}}
                                 :b {#{:c :d} #{b}}
                                 :c {#{} #{c}}
                                 :d {#{} #{d}}}
                ::eql/query     [:a :b]})
      last
      c-nodes-edges))
