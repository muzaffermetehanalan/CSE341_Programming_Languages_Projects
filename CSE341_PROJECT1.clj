; *******************************************
; 341 Programming Languages                *                           
; Author: MUZAFFER METEHAN ALAN -151044038 *
; *******************************************

(use 'clojure.java.io)
(require '[clojure.string])

(def keywords ["and","or","not","equal","append","concat","set","deffun","for","while","if","then","else","defvar"])
(def operators ["+","-","/","*","(",")","'","=","<",">"])
(def binaryvalue ["true","false"])
(def paranthesis ["(",")"," ( "," ) "])


(defn lexerWithOutFile []
		(while true
			(do
			(def tokenList '())
			(print ">") (flush)
			(def interPreterInput (read-line))
			(def interPreterInput (clojure.string/replace interPreterInput (nth paranthesis 0)  (nth paranthesis 2)))
			(def interPreterInput (clojure.string/replace interPreterInput (nth paranthesis 1)  (nth paranthesis 3)))
			(def interPreterInput (clojure.string/split interPreterInput #"\s+"))
			(def interPreterInput(subvec interPreterInput 1))

			(def size (atom 0))
			(while (< @size (count interPreterInput) )
				(do
					(if (not= -1 (.indexOf keywords (nth interPreterInput @size)))
						(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Keyword | " )))
					
						(do
							(if (not= -1 (.indexOf operators(nth interPreterInput @size)))
								(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Operator |" )))
								(do
									(if (not= -1 (.indexOf binaryvalue(nth interPreterInput @size)))
										(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Binary Value | " )))
										(do
											(if (= (type 1)  (type (read-string (nth interPreterInput @size)))) 
												(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Integer Value | " )))
												(do
													(if (> (count (re-seq #"[0-9]+|[a-z]+"  (nth interPreterInput @size))) 1)
														(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Invalid Identifier | " )))
														(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Identifier | " )))

													)


												)											
											)

										)
									)
								)

							)

						)

					)

					(swap! size inc)
				)
			)

			)
		(def tokenList (reverse tokenList))
		(println tokenList)
		)
)

( defn lexer [filename]
		(def givenFile (slurp filename))
		(def givenFile(clojure.string/replace givenFile (nth paranthesis 0)  (nth paranthesis 2)))
		(def givenFile(clojure.string/replace givenFile (nth paranthesis 1)  (nth paranthesis 3)))
		(def givenFile(clojure.string/split givenFile #"\s+"))
		(def interPreterInput (subvec givenFile 1))
		(def tokenList '())
					(def size (atom 0))
			(while (< @size (count interPreterInput) )
				(do
					(if (not= -1 (.indexOf keywords (nth interPreterInput @size)))
						(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Keyword | " )))
					
						(do
							(if (not= -1 (.indexOf operators(nth interPreterInput @size)))
								(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Operator |" )))
								(do
									(if (not= -1 (.indexOf binaryvalue(nth interPreterInput @size)))
										(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Binary Value | " )))
										(do
											(if (= (type 1)  (type (read-string (nth interPreterInput @size)))) 
												(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Integer Value | " )))
												(def tokenList (conj tokenList (str "'" (nth interPreterInput @size) "'" " :" "Identifier | " )))											
												)

										)
									)
								)

							)

						)

					)

					(swap! size inc)
				)
			)

		(def tokenList (reverse tokenList))
		(println tokenList)
		(lexerWithOutFile)

)



(defn lexerMain [] 
	(print "$") (flush)
	(def input (read-line))

	(def input (clojure.string/split input  #"\s+"))

	(if (not= "coffee" (nth input 0))
		(System/exit 0))	

	(if (= 1 (count input))
		(lexerWithOutFile)
		(lexer (nth input 1)))	

)

(lexerMain)

	
