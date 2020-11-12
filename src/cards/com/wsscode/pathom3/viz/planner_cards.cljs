(ns com.wsscode.pathom3.viz.planner-cards
  (:require
    [com.fulcrologic.guardrails.core :refer [<- => >def >defn >fdef ? |]]
    [com.wsscode.pathom3.connect.indexes :as pci]
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
                       (mapv (juxt identity viz-plan/c-nodes-edges)))})))
