(set! *warn-on-reflection* true)

(ns testing)

  (defn- eval-assert
    "Delegation target for all future assert functions."
    [assertion]
    (if assertion
      (println "OK")
      (throw (AssertionError.))))

  (defn assert-equal
    "Either a equals b or an assertion will be thrown"
    [a b]
    (eval-assert (= a b)))

  (defn assert-nil
    "Ensures that *arg* equals nil"
    [arg]
    (assert-equal arg nil))

  (defn assert-not-nil
    "Ensures that *arg* equals not nil"
    [arg]
    (eval-assert (not (nil? arg))))


(ns env-utils)

  (def env-arg (first *command-line-args*))

  (def env (cond
    (some #(= env-arg %) (list "test", "T")) :test
    (some #(= env-arg %) (list "production", "prod", "P")) :prod
    :else
      (do
        (println "Missing environment arg")
        (System/exit -1))))

  (defn mode
    [ & env-with-action]
    ((some #(if (= (first %) env) (second %))
       (partition 2 env-with-action))))

  (defmacro after-initialize
    [ & arg]
    `(let [bye# "run finished!"]
      (println "starting with env: " env/env)
      ~@arg
      (println bye#)))


(ns tree-traversals)
  (alias 'test 'testing)
  (alias 'env 'env-utils)

  (def tree (atom nil))

  (defrecord Node [value left right])

  (defn insert-node
    "Inserts a node into a binary search-tree. Atm the tree is neither balanced nor is insertion tail recursive."
    [value]
    (letfn [(insert-fn [branch]
              (cond
                (nil? branch) (Node. value nil nil)
                (< value (:value branch)) (Node. (:value branch) (insert-fn (:left branch)) (:right branch))
                :else (Node. (:value branch) (:left branch) (insert-fn (:right branch)))))]
      (swap! tree insert-fn)))


  ; "Finds the first node which value matches *search-val*. Uses a binary search which could run in O(log n) but currently
  ; only runs in O(n) => The tree degrades to a linked list in the case the inserted values were already sorted. "
  ; todo: multimethod
  ;(defmulti find-binary-search class)

  (defn find-binary-search
    [search-val]
    (loop [node @tree]
      (cond
        (= search-val (:value node)) node
        (< search-val (:value node))
          (recur (:left node))
        :else
          (recur (:right node)))))



  ; todo: depth-first, breadth-first, in- post- preorder, msp, kruskal usw.


(defn run-tests []
  (test/assert-nil @tree)
  (insert-node 5)
  (test/assert-not-nil @tree)
  (insert-node 2)
  (test/assert-equal 2 (get-in @tree [:left :value]))
  (insert-node 8)
  (test/assert-equal 8 (get-in @tree [:right :value]))
  (insert-node 12)
  (test/assert-equal 12 (get-in @tree [:right :right :value]))
  (test/assert-equal (:value (find-binary-search 12)) 12))


(defn run-production []
  (println "running in production mode..."))

(env/after-initialize
  (env/mode
    :test run-tests
    :prod run-production))




