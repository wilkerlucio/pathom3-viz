(ns com.wsscode.pathom3.viz.planner-cards
  (:require
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [com.wsscode.pathom3.connect.operation :as pco]
    [com.wsscode.pathom3.interface.eql :as p.eql]
    [com.wsscode.pathom3.viz.plan :as viz-plan]
    [edn-query-language.core :as eql]
    [helix.core :refer [$]]
    [nubank.workspaces.card-types.react :as ct.react]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.model :as wsm]))

(ws/defcard plan-view-cytoscape-card
  {::wsm/align ::wsm/stretch-flex}
  (ct.react/react-card
    ($ viz-plan/PlanCytoscape
       {:frames
        (->> (viz-plan/compute-frames
               '{::pci/index-oir {:a {#{:c :b} #{a}}
                                  :b {#{:d} #{bc bc2}}
                                  :c {#{:d} #{bc bc2}}
                                  :e {#{:d} #{bc bc2}}
                                  :d {#{} #{d}}}
                 ::eql/query     [:a]})
             (mapv (juxt identity viz-plan/compute-plan-elements)))})))

(defn run-stats-elements [env tx]
  (viz-plan/compute-plan-elements
    (-> (p.eql/process env tx)
        meta :com.wsscode.pathom3.connect.runner/run-stats
        viz-plan/smart-plan)))

(ws/defcard plan-view-results
  {::wsm/align ::wsm/stretch-flex}
  (ct.react/react-card
    ($ viz-plan/PlanGraphView
       {:elements
        (run-stats-elements
          (pci/register [(pco/resolver 'a
                           {::pco/input [:b :c]
                            ::pco/output [:a]}
                           (fn [_ _]
                             {:a :a}))
                         (pco/resolver 'bc
                           {::pco/input [:d]
                            ::pco/output [:b :c :e]}
                           (fn [_ _]
                             {:b :bc-b
                              :c :bc-c
                              :e :bc-e}))
                         (pco/resolver 'bc2
                           {::pco/input [:d]
                            ::pco/output [:b :c :e]}
                           (fn [_ _]
                             {:b :bc2-b
                              :c :bc2-c
                              :e :bc2-e}))
                         (pco/resolver 'd
                           {::pco/output [:d]}
                           (fn [_ _]
                             {:d :d}))])
          [:a])

        :display-type
        ::viz-plan/display-type-label})))

(ws/defcard plan-view-results-or
  {::wsm/align ::wsm/stretch-flex}
  (ct.react/react-card
    ($ viz-plan/PlanGraphView
       {:elements
        (run-stats-elements
          (pci/register [(pco/resolver `value
                           {::pco/output [:multi-path]}
                           (fn [_ _]
                             {:multi-path 1}))
                         (pco/resolver `value2
                           {::pco/output [:multi-path]}
                           (fn [_ _]
                             {:multi-path 1}))])
          [:multi-path])

        :display-type
        ::viz-plan/display-type-node-id})))

(ws/defcard plan-view-results-or-error
  {::wsm/align ::wsm/stretch-flex}
  (ct.react/react-card
    ($ viz-plan/PlanGraphView
       {:elements
        (run-stats-elements
          (pci/register [(pco/resolver 'error-long-touch
                           {::pco/output [:error]}
                           (fn [_ _]
                             (throw (ex-info "Error" {}))))
                         (pbir/constantly-resolver :error "value")])
          [:error])

        :display-type
        ::viz-plan/display-type-node-id})))
