(ns com.wsscode.pathom3.viz.ui
  (:require [cljs.tools.reader :refer [read-string]]
            [helix.dom :as dom]
            [com.wsscode.misc.coll :as coll]))

(defn state-hook-serialize [[value set-value!]]
  [(pr-str value) #(set-value! (read-string %))])

(defn dom-props [{::keys [state] :as props}]
  (cond-> (coll/filter-keys simple-keyword? props)
    state
    (as-> <>
      (let [[value set-value!] state]
        (assoc <> :value value :on-change #(set-value! (.. % -target -value)))))))

(defn dom-select
  [{::keys [options] :as props}]
  (dom/select {:& (-> props
                      (coll/update-if ::state state-hook-serialize)
                      (dom-props))}
    (for [[value label] options]
      (let [value-str (pr-str value)]
        (dom/option {:value value-str :key value-str} (str label))))))