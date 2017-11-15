(defn c2i
  "Convert character to int, int to character mapping."
  [x]
  (- (int x) (int \a))
  )


(defn i2c
  "Convert int to character."
  [x]
  (char (+ x (int \a)))
  )
