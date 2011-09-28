(ns bin-tree
  (:import (java.util Random)))

(defstruct node :id :value)

(def nodes (ref ())) 

(defn insert-node [node] 
  (dosync 
    ;(ref-set nodes (cons node @nodes))))
    (alter nodes conj node)))

(println @nodes)
(insert-node (struct node 1 99))
(insert-node (struct node 1 44))
(println @nodes)




