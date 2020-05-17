(ns cs441project.core
  (:gen-class))
(require '[com.climate.claypoole :as cp])
(require '[clojure.string :as str])
(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

; Reads the text file to a lazy sequence of a group of chars (length determine by parameter)
; Changes all characters to upper-case, then strips non characters/spaces
(defn readFile [grouping]
  (re-seq (re-pattern (str ".{1," grouping "}")) (str/replace (str/upper-case (slurp ".\\resources\\WarAndPeace.txt")) #"[^A-Z ]" "")))

; Takes in a string and creates a map of characters and their number of occurences in the text
(defn findFrequencies [text]
   (frequencies text))

; Takes in a map of character frequencies and the total number of characters in the file and calculates
; the probability of each character and inserts it into another map
(defn findProbabilityDist [freqs totalChars threads]
  (def probMap (hash-map))
  (cp/pfor threads [key (keys freqs)]
    (def probMap (assoc probMap key (double (/ (get freqs key) totalChars)))))
  probMap)

; Finds the information of a single character from the text
(defn charInfo [charFreq charProb]
  (* charFreq (- charProb) (log2 charProb)))

; Calculates the sum of each character's information
(defn getInformationSum [freqMap probMap threads]
  (def informationTotal 0)
  (cp/pfor threads [key (keys freqMap)]
    (def informationTotal (+ informationTotal (charInfo (get freqMap key) (get probMap key)))))
  informationTotal)

; Controls the process of finding the text's information. Takes in lazy sequence of character "Alphabet"
(defn findTextInformation [textGroups threads]
  (def freqs (findFrequencies textGroups))
  (def probabilities (findProbabilityDist freqs (count textGroups) threads))
  (getInformationSum freqs probabilities threads))

; Runs the program for single, double, and triple character grouping
(defn doProgram [threads]
  (doseq [i [1,2,3]]
	  (def textGroups (readFile i))
    (println i "character splitting")
	  (time(findTextInformation textGroups threads))))

; Runs the program 3 times for each of the specified thread counts
(defn main []
	(doseq [threads [1,2,4,8,16,32,64]]
	  (println threads "threads running:")
	  (doseq [i [1,2,3]]
	    (println "Attempt" i)
	    (doProgram threads))))

(main)