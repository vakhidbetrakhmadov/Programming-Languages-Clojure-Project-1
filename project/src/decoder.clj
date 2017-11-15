(use '(clojure.java io))
(use '[clojure.string :only (split trim join)])

; utility functions
(load-file "include.clj")                                   ;; "c2i and "i2c"

(declare read-as-list spell-checker-0 spell-checker-1 permutate string_form test_gen_decoder_a test_on_test_data
         find_mapping test_mapping get-hey decode encode doc_form read_doc read_dict test_gen_decoder_b0)

; ***********  File readers **********
(defn read_doc
  "Reads a document as a list(document level) of lists (paragraph,new line level) of character lists(word level)"
  [doc_name]
  (if (.exists (file doc_name))
    (doc_form (slurp doc_name))))

(defn read_dict
  "Reads a dictionary containing one word per line as a list of character lists"
  [dict_name]
  (if (.exists (file dict_name))
    (read-as-list dict_name)))

; ----- Helper -----
(defn read-as-list
  "Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
  [filename]
   (map flatten (doc_form (slurp filename))))

(defn doc_form
  "Converts a string to 3 level nested list, a list(document level) of lists (paragraph,new line level) of character lists(word level)"
  [string]
  (apply list
         (map
           (fn [paragraph]
             (apply list
                    (map
                      (fn [word]
                        (apply list
                               (map #(char (first (.getBytes %))) (split word #""))))
                      (split paragraph #"\s+"))))
           (map trim (split string #"\n+")))))

(defn string_form
  "Converts 3 level nested list back to string"
  [doc_form]
  (reduce (fn [n m] (str n "\n" m)) (map (fn [paragraph] (reduce  (fn [n m] (str n " " m )) (map (fn [word] (apply str word)) paragraph))) doc_form)))
; --------- End File readers -----------

; ************* Spell checkers ***********
(defn spell-checker-0
  "Takes a word as character list and a dictionary of words as list of character lists, returns true if word is in the dict"
  [dict word]
  (let [rest_dict (atom dict)]
    (while (and (not-empty @rest_dict)
                (not= (first @rest_dict) word)
                )
      (do (swap! rest_dict rest))
      )
    (= (first @rest_dict) word)))

(defn spell-checker-1
  "Takes a word as character list and a dictionary of words as list of character lists, returns true if word is in the dict"
  [dict word]
  (let [word  (apply str word)
        length (count dict)
        mid_idx (int (/ length 2))
        mid_value (apply str (nth dict mid_idx nil))]
    (if (or (<= length 1) (= mid_value word))
      (= mid_value word)
      (do
        (if (neg? (compare word mid_value))
          (recur (take mid_idx dict) word)
          (recur (drop (inc mid_idx) dict) word))))))
; ------------ End Spell checkers -------


; ************* Permutations generators *************
(defn generate_alphabet_mappings
  "Generates all possible mappings of the english alphabet as a lazy sequence of maps: {a a, b b, ... } {a b, b a, ... }"
  ([]
   (let [letters 26
         alphabet (for [x (range letters)] (i2c x))
         alphabet_mappings (for [permutation (permutate alphabet)]
                             (zipmap alphabet permutation))]
     alphabet_mappings))

  ([most_frequent6]
   "Generates all possible mappings of the english alphabet as a lazy sequence of maps
    with constant 6 most frequent english letters mapped to the supplied 6 ones: {a a, b b, ... } {a b, b a, ... }"
    (let [letters 26
          alphabet (for [x (range letters)] (i2c x))
          natural_most_frequent6 '(\e \t \a \o \i \n)
          alphabet20keys (filter (fn [letter] (every? #(not= % letter) natural_most_frequent6)) alphabet)
          alphabet20values (filter (fn [letter] (every? #(not= % letter) most_frequent6)) alphabet)
          alphabet20_mappings (for [permutation (permutate alphabet20values)]
                                (zipmap alphabet20keys permutation))
          const_mapping (zipmap natural_most_frequent6 most_frequent6)
          alphabet_mappings (for [next alphabet20_mappings]
                              (merge next const_mapping))]
      alphabet_mappings)))

; ---- Helper ---
(defn permutate
  "Generates a lazy sequence of all possible permutations on a given set"
  ([set]
   (permutate '() set))
  ([next_perm set]
   (if (zero? (count set))
     (lazy-seq [next_perm])
     (lazy-seq (apply concat (for [element set] (permutate (conj next_perm element) (remove #{element} set))))))))
; --------------- End Permutations generators --------------

; *************** Mapping finders *********************
(defn find_mapping
  "Sequentially tries all available mappings in order to decode the given paragraph until:
  1) mapping is found (then delivers the result) 2) result was delivered 3) no mappings are left"
  [alphabet_mappings paragraph dict result]
  (let [next (atom (first @alphabet_mappings))]
    (while (and (not (nil? @next)) (not (realized? result)))
      (if (test_mapping @next paragraph dict)
        (deliver result @next)
        (do (swap! alphabet_mappings rest)
            (swap! next (constantly (first @alphabet_mappings))))))))
; ------ Helper -----
(defn get_key
  "Returns a key by its value"
  [map value]
  (keep #(when (= (val %) value) (key %)) map))

(defn test_mapping
  "Checks if the given mapping decodes the given paragraph"
  [mapping paragraph dict]
  (let [decoded_paragraph (map #(decode % mapping ) paragraph)]
    (every? #(spell-checker-1 dict %) decoded_paragraph)))
; ------------------End Mapping finders -------------------

; ***************** Decoders and Encoders *****************
(defn encode_file
  "Encodes a given file with a random mapping from the given range"
  ([filename mappings_range]
   (let [n (rand-int mappings_range)
         alphabet_mappings (generate_alphabet_mappings)
         random_mapping (nth alphabet_mappings n)
         doc (read_doc filename)
         encoder (fn [word] (encode word random_mapping))
         encoded_doc (map #(map encoder %) doc)
         encoded_doc_str (string_form encoded_doc)]
     (spit filename encoded_doc_str)
     encoded_doc_str))

  ([filename most_frequent6 mappings_range]
   "Encodes a given file with a random mapping from the given range"
   (let [n (rand-int mappings_range)
         alphabet_mappings (generate_alphabet_mappings most_frequent6)
         random_mapping (nth alphabet_mappings n)
         doc (read_doc filename)
         encoder (fn [word] (encode word random_mapping))
         encoded_doc (map #(map encoder %) doc)
         encoded_doc_str (string_form encoded_doc)]
     (spit filename encoded_doc_str)
     encoded_doc_str)))

(defn decode
  [word mapping]
  (flatten (map #(get_key mapping %) word)))

(defn encode
  [word mapping]
  (flatten (map #(get mapping %) word)))
; ---------------- End Decoders and Encoders ---------------

; ********************* Frequencies analysis ***************
(defn frequencies_in [seq first]
  "Returns first n most frequent elements in the given list as list"
  (let [flattened_seq (flatten seq)
        frequencies_map (frequencies flattened_seq)
        comparator (fn [key1 key2]
                     (compare [(get frequencies_map key2) key2]
                              [(get frequencies_map key1) key1]))]
    (take first (keys (into (sorted-map-by comparator) frequencies_map)))))
; ------------------- Frequencies analysis ------------------

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A
  [paragraph dict]
  (let [alphabet_mappings (atom (generate_alphabet_mappings))
        result (promise)]

    (dotimes [n 10] (future (find_mapping alphabet_mappings paragraph dict result)))
    (fn [word] (decode word @result))))

(defn Gen-Decoder-B-0
  [paragraph dict]
  (let [most_frequent6 (frequencies_in paragraph 6)
        alphabet_mappings (atom (generate_alphabet_mappings most_frequent6))
        dict (read_dict "dictionary2.txt")
        result (promise)]

    (dotimes [n 10] (future (find_mapping alphabet_mappings paragraph dict result)))
    (fn [word] (decode word @result))))

(defn Gen-Decoder-B-1
  [paragraph dict]

  )

(defn Code-Breaker
  [document decoder]
  (let [decoded_doc (map #(map decoder %) document)]
    (string_form decoded_doc)))

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
  []
  (let [filename "document1.txt"
        file_content "hello\nthis is a test"
        _ (spit filename file_content)
        encoded_file_content (encode_file filename 1000)
        document (read_doc filename)
        dict (read_dict "dictionary1.txt")
        my_comparator #(compare (apply str %1) (apply str %2))
        sorted_dict (sort my_comparator dict)
        decoderA (Gen-Decoder-A (first document) sorted_dict)
        ]
    (println "File content is :\n\n" file_content "\n")
    (println "File content after encoding is :\n\n" encoded_file_content "\n")
    (println "Paragraph representation of the document is :\n\n" document "\n")
    (if (nil? decoderA) (println "Mapping was not found")
                        (println "Decoded text :\n" (Code-Breaker document decoderA)))
    ))

(defn test_gen_decoder_a
  []
  (let [filename "document2.txt"
        file_content "this course covers topics in programming languages and compilers\nattribute grammars and their use in syntax directed translation"
        _ (spit filename file_content)
        encoded_file_content (encode_file filename 1000)
        document (read_doc filename)
        dict (read_dict "dictionary2.txt")
        decoderA (Gen-Decoder-A (first document) dict)]
    (println "File content is :\n\n" file_content "\n")
    (println "File content after encoding is :\n\n" encoded_file_content "\n")
    (println "Paragraph representation of the document is :\n\n" document "\n")
    (if (nil? decoderA) (println "Mapping was not found")
                        (println "Decoded text :\n" (Code-Breaker document decoderA)))))

(defn test_gen_decoder_b0
  []
  (let [filename "document3.txt"
        file_content "good eat meat there there walcott pout pout wee again again vivian this there gin soon there\ntelemann milk home aarhus pouring aaron this waals\npout vivid abbot goal there there inn gun quill"
        _ (spit filename file_content)
        encoded_file_content (encode_file filename '(\t \s \q \p \x \w) 100)
        document (read_doc filename)
        dict (read_dict "dictionary2.txt")
        decoderB0 (Gen-Decoder-B-0 (first document) dict)]
    (println "File content is :\n\n" file_content "\n")
    (println "File content after encoding is :\n\n" encoded_file_content "\n")
    (println "Paragraph representation of the document is :\n\n" document "\n")
    (if (nil? decoderB0) (println "Mapping was not found")
                         (println "Decoded text :\n" (Code-Breaker document decoderB0)))))

(defn -main
  [& args]
  ;; test code...
  (test_on_test_data)
  (test_gen_decoder_a)
  (test_gen_decoder_b0)
  (shutdown-agents))

(-main)