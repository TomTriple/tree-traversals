
(ns testing)

  ; TODO: Gather statistics
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

  (defn nth-arg-is
    "Returns true of the cmd argument at pos *pos* equals *expected*"
    [pos expected]
    (= (nth *command-line-args* pos) expected))

  (defn env-is
    "Returns true if the env argument equals *env*"
    [env]
    (nth-arg-is 0 env))

  (defmacro when-is
    "Only evaluates *env-action* when the env argument equals *env-type*. Each env has a short version e.g. test <=> t <=> T."
    [env-type env-action]
    (and (= env-type :test) (or (env-is "test") (env-is "t"))
      `(~env-action)))


(ns tree-traversals)

  (alias 'test 'testing)
  (alias 'env 'env-utils)
  ;(refer 'testing)
  ;(refer 'env-utils)

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


(defn run-tests []
  (test/assert-nil @tree)
  (insert-node 5)
  (test/assert-not-nil @tree)
  (insert-node 2)
  (test/assert-equal 2 (get-in @tree [:left :value]))
  (insert-node 8)
  (test/assert-equal 8 (get-in @tree [:right :value]))
  (insert-node 12)
  (test/assert-equal 12 (get-in @tree [:right :right :value])))


; TODO: cond-macro
(env/when-is :test run-tests)





