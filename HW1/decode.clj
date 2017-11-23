  ; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yakup Genc                       *
; *********************************************

;; utility functions 
(load-file "include.clj") ;; "c2i and "i2c"


(use 'clojure.java.io)


(defn read-as-list [filename]
   	(with-open [rdr (clojure.java.io/reader filename)]
   	(def documentVector [])
  	(def documentVector (reduce conj documentVector (line-seq rdr))))
   	(def nestedVector [])
	(def tempVector [])
	(def mapStr [])
	(def vecCounter (atom 0))
	(def nestedList '() )

	(while( < @vecCounter (count documentVector))
		(def nestedVecCounter (atom 0))
		(def pharagraphsVector '())
		(do
			(def tempVector (clojure.string/split (nth documentVector @vecCounter) #" "))
			(swap! vecCounter inc)
			(while (< @nestedVecCounter (count tempVector))
				(do
					(def nonMappedWord (nth tempVector @nestedVecCounter))
						(def mapStr (map str nonMappedWord))
						(def charCounter (atom 0))
						(def calculatedVector [])
						(while (< @charCounter (count mapStr))
							(do

								(def calculatedVector (conj calculatedVector (symbol (nth mapStr @charCounter))))
								(swap! charCounter inc)

								)

							)

						(swap! nestedVecCounter inc)
						(def parsedListe (into (list) calculatedVector))
						(def parsedListe(reverse parsedListe))
						(def pharagraphsVector (conj pharagraphsVector parsedListe))

					)


 
				)

			(def pharagraphsVector ( reverse pharagraphsVector))
			(def nestedList (conj nestedList pharagraphsVector))

			))


			(def nestedList ( reverse nestedList))

			nestedList
	)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defn spell-checker-0 
	[word]
	(def dictionaryList (read-as-list "dictionary2.txt"))
	(def controlValue (atom -1))
  	(def dicCounter (atom 0))
  	(def dictionaryList (read-as-list "dictionary2.txt"))
  	(def newWord (conj '() word))
  	(while (< @dicCounter (count dictionaryList ))
  		(do 
  			(if (= newWord (nth dictionaryList @dicCounter))
  				(do
					(def controlValue (atom 1)))

  			)

  			(swap! dicCounter inc)
  		)

  	)
  	(if (= @controlValue 1)
  		(boolean true)
  		(boolean false)
  		)
  	)

	;you should implement this function

(defn spell-checker-1
	[word]
	(def newWord (conj '() word))
	(def dictionaryList (read-as-list "dictionary2.txt"))
 	;you should implement this function
 	(def indValue (.indexOf dictionaryList newWord))
 	( if (> indValue 0)
 			(boolean true) 
 			(boolean false)
 		)
)

(defn helperDecoder [word alphabet]
	(println alphabet)
	(def emptyWord '())
	(def counter (atom 0))
	(while (< @counter (count word))
		(do 
			(println (nth word @counter))
			(def emptyWord ( conj emptyWord (get alphabet (nth word @counter))))
			(swap! counter inc)
			)

		)
	(def emptyWord (reverse emptyWord))
	emptyWord
)


;; -----------------------------------------------------
;; DECODE FUNCTIONS


(defn Gen-Decoder-A 
	[paragraph]

	(def phaCounter (atom 0))
	(def basicAlph '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(def varAlph   '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(def decodedAlph (zipmap  varAlph basicAlph ))

	(while (< @phaCounter (count paragraph))
		(do
			(def pWord (nth paragraph @phaCounter))
			(def wordCounter (atom 0))
			(def calcWord '())

			(while (< @wordCounter (count pWord))
				(do 

					(def calcWord (conj calcWord (get decodedAlph (nth pWord @wordCounter))))
					(swap! wordCounter inc)
					)

				)
				(reverse calcWord)

			(if (= (spell-checker-1 calcWord) true)
				(do
					(swap! phaCounter inc)
					)

				(do
	 				(def decodedAlph (zipmap  (shuffle varAlph) basicAlph )) 
	 				(def phaCounter (atom 0)

	 				))
 				)
			)

		)

	(defn inline-fn [w]
		(helperDecoder w decodedAlph))
		inline-fn
	
)

(defn Gen-Decoder-B-0 
	[paragraph]
	(def mostFreq '(e t a o i n))
	(def basicAlph '(b c d f g h j k l m p q r s  u v w x y z))
	(def varAlph   '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	(def freqCounter '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
	(def alphWithCounter (zipmap varAlph freqCounter))
	(def pharagCounter (atom 0))

	(while (< @pharagCounter (count paragraph))
		(do
			 (def wordCounter (atom 0))
				(while (< @wordCounter (count (nth paragraph @pharagCounter)))
					(do
						(def alphWithCounter ( update alphWithCounter (nth (nth paragraph @pharagCounter) @wordCounter ) inc))
						(swap! wordCounter inc)
						)

					)

			(swap! pharagCounter inc)
			)

		)

	(def sortedFreq (sort-by second > alphWithCounter))
	(def mostFreqCipher '() )
	(def fCounter (atom 0))
	(while (< @fCounter 6)
		(do
			(def mostFreqCipher (conj mostFreqCipher (nth (nth sortedFreq @fCounter)0)))
			(swap! fCounter inc)

			)

		)

	(def mostFreqCipher (reverse mostFreqCipher))
	(def varAlph(remove (set mostFreqCipher) varAlph))
	(def mostFreqMap (zipmap mostFreqCipher mostFreq))
	(def leastFreqMap (zipmap varAlph basicAlph))
	(def decodedAlph (merge mostFreqMap leastFreqMap))
	(def phaCounter (atom 0))

		(while (< @phaCounter (count paragraph))
		(do
			(def pWord (nth paragraph @phaCounter))
			(def wordCounter (atom 0))
			(def calcWord '())

			(while (< @wordCounter (count pWord))
				(do 

					(def calcWord (conj calcWord (get decodedAlph (nth pWord @wordCounter))))
					(swap! wordCounter inc)
					)

				)
				(reverse calcWord)

			(if (= (spell-checker-1 calcWord) true)
				(do
					(swap! phaCounter inc)
					)

				(do
					(def mostFreqCipher (shuffle mostFreqCipher))
					(def mostFreqMap (zipmap mostFreqCipher mostFreq))
					(def varAlph (shuffle varAlph))
					(def leastFreqMap (zipmap varAlph basicAlph))
					(def decodedAlph (merge mostFreqMap leastFreqMap))
	 				(def phaCounter (atom 0)

	 				))
 				)
			)

		)
			(defn inline-fn [w]
		(helperDecoder w decodedAlph))
		inline-fn
)

(defn Gen-Decoder-B-1 
	[paragraph]
  	;you should implement this function
)

(defn Code-Breaker 
	[document decoder]
	(def funcPtr (decoder (nth document 0)))
	(def docCount (atom 0))
	(def phaList '())
	(def docList '())
	(while (< @docCount (count document))
		(do 
			(def wordCount (atom 0))
			(while (< @wordCount (count (nth document @docCount)))
				(do
					(def phaList (conj phaList (funcPtr (nth (nth document @docCount) @wordCount))))
					(swap! wordCount inc)
					)

				)

			(def phaList (reverse phaList))

			(def docList (conj docList phaList))
			(def docList (reverse docList))
			(swap! docCount inc)
			)

		)
)

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[]
		(let [doc (read-as-list "document1.txt")]
		(println doc)
		(println "Gen-Decoder-A = " (Code-Breaker doc Gen-Decoder-A))
		(println "Gen-Decoder-B-0= " (Code-Breaker doc Gen-Decoder-B-0))
		))
;; test code...
(test_on_test_data)




( defn lexer [filename]
		(def e (slurp filename))
		(def a "(")
		(def b ")")
		(def c " ( ")
		(def d " ) ")
		(def e (clojure.string/replace e a  c))
		(def e (clojure.string/replace e b  d))
		(def e(clojure.string/split e  #"\s+"))
		(def e (subvec e 1))
		(println e)

		)
