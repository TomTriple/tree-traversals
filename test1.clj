
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
    "Only evaluates *env-action* when the env argument equals *env-type*. Each env has a short version e.g. test <=> t <=> T."
    [env-type env-action]
    (and (= env-type :test) (or (env-is "test") (env-is "t")) 
      `(~env-action)))


(ns tree-traversals)
  (refer 'testing)
  (refer 'env-utils)

  (defstruct node-item :id :value :left :right)
  (def root (ref nil))
  
  (defn- belongs-left? 
    "Returns true, if node-to-insert´s value key is greater than node´s value. "
    [node node-to-insert] 
    (> (node-to-insert :value) (node :value)))
    
  (defn- belongs-right?
    "Is defined in terms of belongs-left? and returns the inverted boolean value. "
    [node node-to-insert]
    (not (belongs-left? node node-to-insert)))

  (defn insert-node
    "Inserts a node to a binary, non-balanced search-tree."
    [node-to-insert]
    (if @root
      (dosync 
        (loop [node @root] 
          (if (belongs-left? node node-to-insert) 
            (if (node :left) 
              (recur (node :left))
              (assoc node :left node-to-insert))
            (if (node :right) 
              (recur (node :right)) 
              (assoc node :right node-to-insert)))))
      (dosync 
        (ref-set root node-to-insert))))


(defn run-tests []
    (assert-nil @root)
    (insert-node (struct node-item 1 99))
    (insert-node (struct node-item 2 105))    
    (insert-node (struct node-item 3 88)))


(when-env-is :test run-tests)
(println @root)




