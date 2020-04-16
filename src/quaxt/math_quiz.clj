(ns quaxt.math-quiz
  (:require [clojure.string :as str])
  (:gen-class))

(defn as-int
  [x]
  (if (string? x)
    (Integer/parseInt x)
    x))

(defn as-double
  [x]
  (if (string? x)
    (Double/parseDouble x)
    x))

(defn all-questions
  []
  (shuffle (concat
            (for [op ["+" "*"]
                  x (range 0 13)
                  y (range 0 13)
                  ]
              [op x y])

            (for [
                  x (range 0 13)
                  y (range 0 x)
                  ]
              ["-" x y])
            (for [
                  x (range 0 13)
                  y (range 1 13)
                  ]
              ["/" (* x y) y]))))

(defn math-file[]
  (str/join
   java.io.File/separator
   [(System/getProperty "user.home") "mathdata.txt"]))

(defn right-answer
  "takes a [op [x y] time answer] and returns true iff x op y = answer"
  [op x y answer & rest]
  (let [op (if (string? op) (resolve (symbol op)) op)]
    (= (str (op x y)) answer)))

(defn read-math-data[]
  (with-open [rdr (clojure.java.io/reader (math-file))]
    (into [] (map (fn[line]
                    (let [[op x y answer time]
                          (clojure.string/split
                           line #" ")
                          x (as-int x)
                          y (as-int y)
                          time (as-double time)]
                      [op x y answer time])
                    ) (line-seq rdr)))))

(defn cmp-nth-desc[n]
  (fn[a b] (compare (nth b n) (nth a n))))

(defn read-difficulty[math-data]
  (let [difficulty 
        (into [] (map (fn[[op x y answer time]]
                        (let [right (right-answer (resolve (symbol op)) x y answer)
                              difficulty (if right (as-double time) 1000000000) ]
                          [op x y difficulty])
                        ) math-data))]
    (sort (cmp-nth-desc 3) difficulty)))

(defn questions-by-difficulty[math-data]
  (sort (fn[[_ d1] [_ d2]] (compare d2 d1))
        (reduce (fn[m [k v]] (assoc m k v))
                {}
                (concat
                 (map (fn[x] [x 1000000]) (all-questions))
                 (map (fn[[op x y difficulty]]
                        [[op x y] difficulty]) (read-difficulty math-data))))))

(defmacro with-time
  "Evaluates expr. Returns the pair of how long it took to evaluate and value of
  expr."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#]))

;; ask 100 questions record answers and how long they took
(defn op-name
  [op]
  (if (string? op)
    op
    (:name (meta op))))

(defn question
  "Ask a question then return the question, how long it took and the answer given"
  [op x y]
  (let [[time answer] (with-time
                        (do (print x op y "= ")
                            (flush)
                            (read-line)))]
    (when-not (= answer "quit")
      [op x y answer time])))

(defn quiz
  "Ask n questions and return vecor of each question, answer and how it
  took to answer if op is not nil then question should be on that
  op (e.g. + - / *), otherwise pick op at random for each question
  "
  [op n math-data]
  (let [questions (questions-by-difficulty math-data)
        questions (if op (filter
                          (fn [[qop x y] _] (= op qop))
                          questions)
                      questions)]
    (into [] (take n (take-while identity (map (fn [[q _]] (apply question q)) questions))))))

(defn analysis
  "return the results in order of which were right or wrong (wrong
  first) and then how long it took to answer"
  [results]
  (def zzz results)
  (let [sorted (sort (fn[x y]
                       (let [right-x (apply right-answer x)
                             right-y (apply right-answer y)
                             time-x (nth x 4)
                             time-y (nth y 4)]
                         (cond (= right-x right-y) (compare time-y time-x)
                               right-x 1
                               right-y -1))) results)]
    sorted))

(defn format-result[[op x y & rest]]
  (str (op-name op) \space x \space y \space (str/join " " rest)))

(defn right-cmp[a b]
  (let [ra (apply right-answer a)
        rb (apply right-answer b)]
    (cond
      (= ra rb) 0
      ra 1
      rb -1)))

(defn and-then[cmp1 cmp2]
  (fn[a b]
    (let [c1 (cmp1 a b)]
      (if (zero? c1) (cmp2 a b) c1))))

(defn merge-data
  [old-data new-data]
  (sort (and-then right-cmp (cmp-nth-desc 4)) (vals (reduce
                                (fn[m [op x y answer time :as line]]
                                  (assoc m [op x y] line))
                                {}
                                (concat old-data new-data)))))

(defn -main[& args]
  (let [n (as-int (or (first args) 10))
        op (second args)
        op (when op (resolve (symbol op)))
        math-data (read-math-data)
        results (quiz op n math-data)
        analyzed (analysis results)
        merged (merge-data math-data analyzed)]
    (spit (math-file) (str/join \newline (map (fn[line] (str/join \space line)) merged)))
    (doseq [result analyzed]
      (let [formatted (format-result result)]
        (println formatted (apply right-answer result))))))
