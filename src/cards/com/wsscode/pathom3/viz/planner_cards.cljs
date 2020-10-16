(ns com.wsscode.pathom3.viz.planner-cards
  (:require [nubank.workspaces.card-types.react :as ct.react]
            [nubank.workspaces.core :as ws]
            [nubank.workspaces.model :as wsm]
            [helix.dom :as dom]))

(ws/defcard plan-view-dagre-card
  (ct.react/react-card
    (dom/div "Hello World")))
