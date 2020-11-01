(ns com.wsscode.pathom3.viz.planner-cards
  (:require
    [cljs.tools.reader :refer [read-string]]
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.pathom3.viz.plan :as viz-plan]
    [edn-query-language.core :as eql]
    [com.wsscode.pathom3.connect.planner :as pcp]
    [com.wsscode.pathom3.connect.indexes :as pci]
    [helix.core :refer [$]]
    [nubank.workspaces.card-types.react :as ct.react]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.model :as wsm]
    [helix.hooks :as hooks]))

(ws/defcard plan-view-dagre-card
  {::wsm/align ::wsm/align-top-flex}
  (ct.react/react-card
    ($ viz-plan/PlanView
      {:frames
       (->> (viz-plan/frames
              #_'{::pci/index-oir      {:a {#{} #{a}}
                                        :b {#{:g} #{b}}
                                        :c {#{} #{c}}
                                        :f {#{:e} #{f}}
                                        :g {#{:c :f} #{g}}
                                        :h {#{:a :b} #{h}}}
                  ::pcp/available-data {}
                  ::eql/query          [:h]}
              #_'{::pci/index-oir {:a {#{:c} #{a}}
                                   :b {#{:d} #{b}}
                                   :c {#{} #{cd}}
                                   :d {#{} #{cd d}}}
                  ::eql/query     [:a :b]}
              '{::pci/index-oir {:a {#{:c :d} #{a}}
                                 :b {#{:c :d} #{b}}
                                 :c {#{} #{c}}
                                 :d {#{} #{d}}}
                ::eql/query     [:a :b]}
              #_'{::pci/index-oir {:a {#{:c :b} #{a}}
                                   :b {#{:d} #{bc bc2}}
                                   :c {#{:d} #{bc bc2}}
                                   :e {#{:d} #{bc bc2}}
                                   :d {#{} #{d}}}
                  ::eql/query     [:a]})
            (mapv (juxt identity viz-plan/layout-graph)))})))



(ws/defcard plan-view-cytoscape-card
  {::wsm/align ::wsm/stretch-flex}
  (ct.react/react-card
    (let [source-state (hooks/use-state '{::pci/index-oir {:a {#{:c :b} #{a}}
                                                           :b {#{} #{b}}
                                                           :c {#{} #{c}}
                                                           :d {#{:c :b} #{d}}
                                                           :e {#{:c :b :f} #{e}}
                                                           :f {#{} #{f}}}
                                          ::eql/query     [:a :d :e]})]
      ($ viz-plan/PlanCytoscape
        {:frames
         (->> (viz-plan/frames
                '{::pci/index-oir {:a {#{:c :b} #{a}}
                                   :b {#{} #{b}}
                                   :c {#{} #{c}}
                                   :d {#{:c :b} #{d}}
                                   :e {#{:c :b :f} #{e}}
                                   :f {#{} #{f}}}
                  ::eql/query     [:a :d :e]})
              (mapv (juxt identity viz-plan/c-nodes-edges)))}))))
