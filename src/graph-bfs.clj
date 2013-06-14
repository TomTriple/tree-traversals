(defrecord Edge [weight target])
(defrecord Node [label edges])

(def graph (atom {}))

(defn node-connect
  "Connects the start node :src to node :dst with edge weight :weight. This fn supports arity overloading."
  ([src dst weight]
    (let [src-node (src @graph)
          new-edges (conj (:edges src-node) (Edge. weight dst))]
      (swap! graph assoc src
        (assoc src-node :edges new-edges))))
  ([src dst weight & args]
    (doseq [x (partition 3 (apply list src dst weight args))]
      (node-connect (nth x 0) (nth x 1) (nth x 2)))))


(defn node-add
  "Adds a node to the graph with an empty list of adjacent nodes."
  [ & ids]
  (swap! graph assoc reduce #(assoc %1 (Node. %2 [])) {} ids))


(defn node-degree
  "Returns the degree of a node."
  [node]
  (count (:edges node)))


(defn graph-bfs
  "Performs a breadth first search (bfs) on :graph starting at :p. Returns a bfs tree datastructure as map."
  [graph start]
  (loop [q [start]
         states {start :discovered}
         tree {start -1}]
    (if-not (seq q)
      tree
      (do
        (println "discovered: " (peek q))
        (let [adj (vec (for [e (:edges ((peek q) graph)) :when  (not (contains? states (:target e)))] (:target e)))]
          (recur
            (vec
              (flatten (cons adj (pop q))))
            (reduce #(assoc %1 %2 :discovered) states adj)
            (reduce #(assoc %1 %2 (peek q)) tree adj)))))))


(defn graph-dfs
  "Performs a depth first search (dfs) on :graph starting at :start. "
  [start]
  (loop [stack [start]
         states {start :discovered}]
    (when-let [_ (seq stack)]
      (println "discovered: " (first stack))
      (let [adj (vec (for [e (:edges ((first stack) @graph)) :when (not (contains? states (:target e)))] (:target e)))]
        (recur
          (flatten (conj adj (rest stack)))
          (reduce #(assoc %1 %2 :discovered) states adj))))))


(defn bfs-find-path
  "Finds the shortests path from :start to :end."
  [tree start end]
  (loop [start start
         path []]
    (if (or (= start end) (end -1))
      (conj path end)
      (recur
        (get tree start)
        (conj path start)))))


(node-add :grhh :bra :feilnbach :aibling :raubling :rosenheim :wasserburg)

(node-connect
  :grhh :raubling 5
  :grhh :bra 2
  :bra :grhh 2
  :grhh :feilnbach 10
  :feilnbach :aibling 5
  :aibling :rosenheim 7
  :raubling :rosenheim 10
  :rosenheim :wasserburg 30
)

(println "depth first search: ")
(graph-dfs :grhh)

(println "breadth first search: ")

(let [graph-bfs-tree (graph-bfs @graph :bra)]
  (println
    (str "path from :wasserburg to :bra -> " (bfs-find-path graph-bfs-tree :wasserburg :bra))))




