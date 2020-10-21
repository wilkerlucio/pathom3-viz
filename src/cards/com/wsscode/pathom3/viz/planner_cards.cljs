(ns com.wsscode.pathom3.viz.planner-cards
  (:require
    ["dagre" :as dagre]
    [applied-science.js-interop :as j]
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.connect.planner :as pcp]
    [com.wsscode.pathom3.entity-tree :as p.ent]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.interface.smart-map :as psm]
    [helix.core :as h :refer [$]]
    [helix.dom :as dom]
    [helix.hooks :as hooks]
    [nubank.workspaces.card-types.react :as ct.react]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.model :as wsm]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [clojure.spec.alpha :as s]
    [com.wsscode.pathom3.connect.runner.stats :as pcrs]))

(pco/defresolver node-type [env _]
  {::pco/input [::pcp/node-id]}
  {::pcp/node-type (pcp/node-kind (p.ent/entity env))})

(pco/defresolver node-label [env {::pcp/keys [node-id node-type]}]
  {::node-label
   (str node-id " | "
     (case node-type
       ::pcp/node-and "AND"
       ::pcp/node-or "OR"
       ::pcp/node-resolver (::pco/op-name (p.ent/entity env))))})

(pco/defresolver node-color [{::pcp/keys [node-id node-type]}]
  {::node-color
   (case node-type
     ::pcp/node-and "yellow"
     ::pcp/node-or "cyan"
     ::pcp/node-resolver "gray")})

(def node-extensions-registry [node-type node-label node-color])
(def node-extensions-indexes
  (-> (pci/register node-extensions-registry)
      (psm/with-keys-mode ::psm/keys-mode-reachable)))

;;;;;;;;;;;;;

(>def :repl.runtime/namespace symbol?)
(>def :repl.runtime/ns-aliases map?)
(>def :repl.runtime/ns-interns map?)

(>def :repl.runtime.sexp.symbol/alias-ns string?)
(>def :repl.runtime.sexp.symbol/name string?)
(>def :repl.runtime.sexp.symbol/fq-ns symbol?)
(>def :repl.runtime.sexp.symbol/symbol symbol?)
(>def :repl.runtime.sexp.symbol/fq-symbol symbol?)
(>def :repl.runtime.sexp.symbol/ns-interns symbol?)
(>def :repl.runtime.sexp.symbol/var var?)
(>def :repl.runtime.sexp.symbol/var-meta map?)
(>def :repl.runtime.sexp.symbol/meta-filename string?)

(pco/defresolver ns-aliases-resolver [{:repl.runtime/keys [namespace]}]
  {:repl.runtime/ns-aliases {}})

(pco/defresolver ns-interns-resolver [{:repl.runtime/keys [namespace]}]
  {:repl.runtime/ns-interns {}})

(pco/defresolver sexp-symbol-alias-ns [{:repl.runtime.sexp.symbol/keys [symbol]}]
  {:repl.runtime.sexp.symbol/alias-ns (namespace symbol)})

(pco/defresolver sexp-symbol-name [{:repl.runtime.sexp.symbol/keys [symbol]}]
  {:repl.runtime.sexp.symbol/name (name symbol)})

(pco/defresolver sexp-symbol-namespace
  [{:repl.runtime.sexp.symbol/keys [alias-ns]
    :repl.runtime/keys             [ns-aliases]}]
  {:repl.runtime.sexp.symbol/fq-ns (symbol (str (get ns-aliases (symbol alias-ns))))})

(pco/defresolver sexp-ns-interns-resolver [{:repl.runtime.sexp.symbol/keys [fq-ns]}]
  {:repl.runtime.sexp.symbol/ns-interns {}})

(pco/defresolver sexp-fq-symbol [{:repl.runtime.sexp.symbol/keys [fq-ns name]}]
  {:repl.runtime.sexp.symbol/fq-symbol (symbol (str fq-ns) name)})

(pco/defresolver sexp-var-from-symbol [{:repl.runtime.sexp.symbol/keys [name ns-interns]}]
  {:repl.runtime.sexp.symbol/var (get ns-interns (symbol name))})

(pco/defresolver sexp-var-meta [{:repl.runtime.sexp.symbol/keys [var]}]
  {:repl.runtime.sexp.symbol/var-meta (meta var)})

(pco/defresolver meta-filename [{:repl.runtime.sexp.symbol/keys [var-meta]}]
  {:repl.runtime.sexp.symbol/meta-filename var-meta})

(def indexes-demo
  (pci/register
    [ns-aliases-resolver
     ns-interns-resolver
     sexp-symbol-alias-ns
     sexp-symbol-name
     sexp-symbol-namespace
     sexp-ns-interns-resolver
     sexp-fq-symbol
     sexp-var-meta
     sexp-var-from-symbol
     meta-filename]))

(comment
  (let [env  (p.ent/with-entity indexes-demo
               {:repl.runtime/namespace          'com.wsscode.pathom3.demos.clojure-meta
                :repl.runtime.sexp.symbol/symbol 'pci/register})
        res  (-> env
                 (p.eql/process [:repl.runtime.sexp.symbol/ns-interns
                                 :com.wsscode.pathom3.connect.runner/run-stats]))
        plan (-> res :com.wsscode.pathom3.connect.runner/run-stats)]
    (render-graph plan env)))

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

(defn render-graph [graph env]
  graph)

(defn compute-plan []
  (let [env  (p.ent/with-entity indexes-demo
               {:repl.runtime/namespace          'com.wsscode.pathom3.demos.clojure-meta
                :repl.runtime.sexp.symbol/symbol 'pci/register})
        res  (-> env
                 (p.eql/process [:repl.runtime.sexp.symbol/var
                                 :com.wsscode.pathom3.connect.runner/run-stats]))
        plan (-> res :com.wsscode.pathom3.connect.runner/run-stats)]
    (psm/sm-update-env plan pci/register node-extensions-indexes)))

(defn compute-frames []
  (let [env        (p.ent/with-entity indexes-demo
                     {:repl.runtime/namespace          'com.wsscode.pathom3.demos.clojure-meta
                      :repl.runtime.sexp.symbol/symbol 'pci/register})
        snapshots* (atom [])
        res        (-> env
                       (assoc ::pcp/snapshots* snapshots*)
                       (p.eql/process [:repl.runtime.sexp.symbol/var
                                       :com.wsscode.pathom3.connect.runner/run-stats]))
        plan       (-> res :com.wsscode.pathom3.connect.runner/run-stats)
        frames     (-> (mapv #(psm/smart-map node-extensions-indexes %) @snapshots*)
                       (conj (psm/sm-update-env plan pci/register node-extensions-indexes)))]
    frames))

(comment
  (-> indexes-demo ::pci/index-attributes keys (->> (remove set?)))
  (let [env  (p.ent/with-entity indexes-demo
               {:repl.runtime/namespace          'com.wsscode.pathom3.demos.clojure-meta
                :repl.runtime.sexp.symbol/symbol 'pci/register})
        res  (-> env
                 (p.eql/process [:repl.runtime.sexp.symbol/var
                                 :com.wsscode.pathom3.connect.runner/run-stats]))
        plan (-> res :com.wsscode.pathom3.connect.runner/run-stats)]
    (js/console.log "!! plan" (.nodes (layout-graph plan))))

  (new-graph)
  (meta (get (ns-interns 'com.wsscode.pathom3.connect.indexes) 'resolver)))

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

(comment
  (js/console.log "!! " (-> (compute-plan)
                            (::pcp/nodes)
                            (vals)
                            first
                            ::node-color)))

(h/defnc PlanView [{:keys [frames]}]
  (let [[current-frame set-current-frame] (hooks/use-state (dec (count frames)))
        [graph data] (get frames current-frame)]
    (dom/div
      (dom/div
        (dom/input
          {:type      "range"
           :min       0
           :max       (dec (count frames))
           :step      1
           :value     current-frame
           :on-change #(set-current-frame (js/parseInt (.. % -target -value)))}))
      (dom/svg {:width 500 :height 500}
        (for [item (mapv #(.node data %) (.nodes data))
              :let [{:keys [id root label color width height x y]} (j/lookup item)]]
          (h/<>
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
              (str id))))

        (for [item (mapv #(.edge data %) (.edges data))
              :let [{:keys [points runNext]} (j/lookup item)
                    p0 (j/lookup (first points))
                    p1 (j/lookup (last points))]]
          (dom/path {:key   (str "path-" (js/JSON.stringify item))
                     :style {:fill   "none"
                             :stroke (if runNext "#000" "#ff9517")}
                     :d     (create-path-curve
                              {::x (:x p0) ::y (:y p0)}
                              {::x (:x p1) ::y (:y p1)})})))
      (dom/div {:style {:display "flex"}}
        (dom/div {:style {:marginRight "2rem"}}
          (for [[attr node-id] (sort-by first (::pcp/index-attrs graph))]
            (dom/div (pr-str attr))))
        (dom/div
          (for [[resolver node-id] (sort-by first (::pcp/index-resolver->nodes graph))]
            (dom/div (pr-str resolver))))))))

(ws/defcard plan-view-dagre-card
  {::wsm/align ::wsm/align-top-flex}
  (ct.react/react-card ($ PlanView {:frames
                                    (mapv (juxt identity layout-graph)
                                      (dedupe (compute-frames)))})))

(comment
  (let [g (layout-graph (compute-plan))]
    (js/console.log "!! " (map #(.edge g %) (.edges g))))

  (map identity nil)
  (psm/sm-keys (psm/sm-env (psm/smart-map {} {})))
  (psm/smart-map {} {})

  (cljs.pprint/pprint
    (compute-plan))

  (s/explain ::pcp/node
    (-> (compute-plan)
        ::pcp/nodes
        (get 1)
        #_(psm/sm-update-env psm/with-keys-mode ::psm/keys-mode-cached)
        ))

  (let [env        (p.ent/with-entity indexes-demo
                     {:repl.runtime/namespace          'com.wsscode.pathom3.demos.clojure-meta
                      :repl.runtime.sexp.symbol/symbol 'pci/register})
        snapshots* (atom [])
        res        (-> env
                       (assoc ::pcp/snapshots* snapshots*)
                       (p.eql/process [:repl.runtime.sexp.symbol/var
                                       :com.wsscode.pathom3.connect.runner/run-stats]))
        plan       (-> res :com.wsscode.pathom3.connect.runner/run-stats)]
    (psm/sm-update-env plan pci/register node-extensions-registry)
    snapshots*)

  (cljs.pprint/pprint
    (compute-frames))

  (cljs.pprint/pprint
    (let [env  (pci/register (pbir/constantly-resolver :foo "bar"))
          res  (-> env
                   (p.ent/with-entity {})
                   (p.eql/process [:foo
                                   :com.wsscode.pathom3.connect.runner/run-stats]))
          plan (-> res :com.wsscode.pathom3.connect.runner/run-stats)]
      (-> plan
          ::pcp/nodes
          (get 1))))
  (into {})
  (map #(into {} %) (vals (::pcp/nodes (compute-plan)))))
