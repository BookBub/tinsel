(ns tinsel.css
  (:require [clojure.zip :as zip]
            [tinsel.core :refer :all])
  (:import [org.w3c.css.sac InputSource CombinatorCondition Condition
            ConditionalSelector DescendantSelector ElementSelector
            Selector SiblingSelector SimpleSelector]
           [java.io StringReader]
           [com.steadystate.css.parser CSSOMParser]))

(defn str->selector
  "Turn a str of a css selector, `table.foo`, into a org.w3c.css.sac.Selector"
  [css-selector-str]
  (.item (.parseSelectors (CSSOMParser.) (InputSource. (StringReader. css-selector-str))) 0))

(defprotocol TinselSelectable
  (->tinsel-selector [selectable] "Return a tinsel selector based on a css selector"))

(extend ElementSelector
  TinselSelectable
  {:->tinsel-selector
   (fn [^ElementSelector selector]
     (if (nil? (.getLocalName selector))
       identity
       (tag= (.getLocalName selector))))})

(extend ConditionalSelector
  TinselSelectable
  {:->tinsel-selector
   (fn [^ConditionalSelector selector]
     (every-selector (->tinsel-selector (.getSimpleSelector selector))
                     (->tinsel-selector (.getCondition selector))))})

(extend Condition
  TinselSelectable
  {:->tinsel-selector
   (fn [^Condition condition]
     (condp = (.getConditionType condition)
       Condition/SAC_ID_CONDITION (id= (.getValue condition))
       Condition/SAC_CLASS_CONDITION (has-class? (.getValue condition))))})

(extend DescendantSelector
  TinselSelectable
  {:->tinsel-selector
   (fn [^DescendantSelector selector]
     (let [parent (->tinsel-selector (.getAncestorSelector selector))
           child (->tinsel-selector (.getSimpleSelector selector))]
       (if (= Selector/SAC_CHILD_SELECTOR (.getSelectorType selector))
         (select parent child)
         (select (or-ancestor parent) child))))})

(extend SiblingSelector
  TinselSelectable
  {:->tinsel-selector
   (fn [^SiblingSelector selector]
     (let [prev-sibling (->tinsel-selector (.getSelector selector))
           to-match (->tinsel-selector (.getSiblingSelector selector))]
       (every-pred to-match zip/left (comp prev-sibling zip/left))))})

(extend CombinatorCondition
  TinselSelectable
  {:->tinsel-selector
   (fn [^CombinatorCondition selector]
     (every-selector (->tinsel-selector (.getFirstCondition selector))
                     (->tinsel-selector (.getSecondCondition selector))))})

(extend String
  TinselSelectable
  {:->tinsel-selector (comp ->tinsel-selector str->selector)})

(extend clojure.lang.IFn TinselSelectable {:->tinsel-selector identity})

(extend clojure.lang.Keyword TinselSelectable {:->tinsel-selector (comp ->tinsel-selector name)})

(extend Object TinselSelectable {:->tinsel-selector (constantly identity)})
