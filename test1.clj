
(ns testing)

  (defn- eval-assert
    "Delegation target for all future assert functions"
    [assertion]
    (if assertion
      (println "OK")
      (throw (AssertionError.))))

  (defn assert-equal
    "Either a equals b or an assertion will be thrown"
    [a b]
    (eval-assert (= a b)))


(ns env-utils)

  (defn nth-arg-is 
    "Returns true of the cmd argument at pos *pos* equals *expected*"
    [pos expected]
    (= (nth *command-line-args* pos) expected))
    
  (defn env-is 
    "Returns true if the env argument equals *env*"
    [env]
    (nth-arg-is 0 env))
  
  (defmacro when-env-is 
    "Only evaluates *env-action* when the env argument equals *env-type*. Each env has a short version e.g. test <=> t."
    [env-type env-action]
    (and (= env-type :test) (or (env-is "test") (env-is "t")) 
      `(~env-action)))


(ns tree-traversals
  (:use 
    (testing) (env-utils))
  (:import (java.util Random)))

  (defstruct node :id :value)
  (def nodes (atom ()))
  (defn insert-node [node]
    (swap! nodes conj node))


  (defn run-tests [] 
    (do 
      (insert-node (struct node 1 99))
      (insert-node (struct node 1 44))
      (testing/assert-equal (count @nodes) 2)
      (println @nodes)))

  (env-utils/when-env-is :test run-tests)

