(def foo :int 42)
(def add (:fun :int :int :int)
  (fn [x :int y :int] (+ x y)))
(println (add foo 42))
