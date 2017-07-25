(ns tinsel.test.css
  (:require [tinsel
             [css :refer :all]
             [core :refer :all]
             [utils :refer [normalize-form]]
             [zip :refer [hiccup-zip]]]
            [clojure.zip :as zip]
            [clojure.test :refer :all]))

(defn zip->seq
  [loc]
  (take-while (complement zip/end?) (iterate zip/next loc)))

(def hiccup->nodes (comp zip->seq hiccup-zip normalize-form))

(defn attributes
  [zipper]
  (-> zipper zip/node (nth 1)))

(defn correct-results
  [expected-correct selector form]
  (let [matches (filter (->tinsel-selector selector) (hiccup->nodes form))]
    (= (take expected-correct (repeat "correct")) (map (comp :id attributes) matches))))

(deftest css->selector-test
  (testing "matching by tag name"
    (is (correct-results 3 "div" [:html [:div#correct] [:div#correct] [:div#correct]])))
  (testing "matching by class name"
    (is (correct-results 2 ".foo" [:html [:div#correct.foo] [:div] [:div#correct.foo]])))
  (testing "matching by id"
    (is (correct-results 3 "#correct" [:html [:div#correct.foo] [:div#correct] [:div#correct.foo]])))
  (testing "child selectors"
    (is (correct-results 1
                         ".bar > .foo" [:html [:div.bar [:div#correct.foo] [:div [:div.foo]]] [:div.foo]])))
  (testing "combinination selectors div.foo.bar"
    (is (correct-results 1 ".foo.bar" [:html [:div#correct.foo.bar] [:div] [:div.foo]])))
  (testing "ancestor selectors"
    (is (correct-results 2 ".bar .foo" [:html [:div.bar [:div#correct.foo] [:div [:div#correct.foo]]] [:div.foo]])))
  (testing "sibling selectors div.bar + div.foo"
    (is (correct-results 1 ".bar + .foo" [:html [:div.bar [:div.foo] [:div [:div.foo]]] [:div#correct.foo]])))
  (testing "keyword matching"
    (testing "matching by tag name"
      (is (correct-results 3 :div [:html [:div#correct] [:div#correct] [:div#correct]])))
    (testing "matching by class name"
      (is (correct-results 2 :div.foo [:html [:div#correct.foo] [:div] [:div#correct.foo]])))
    (testing "matching by id"
      (is (correct-results 1 :div#correct [:html [:div#correct.foo] [:div] [:div.foo]])))))
