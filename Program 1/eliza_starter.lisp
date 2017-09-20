;;==========================================================================
;;
;; STARTER FILE FOR CSC 4240/5240 PROGRAM #1: Eliza
;;==========================================================================

;;----------------------------------------------------------------------------
;; eliza: top-level function which, when given a sentence (no
;; punctuation, please!), comes back with a response like you would.

( defun eliza ( sentence )
  ( respond ( change-pros sentence ) database ))

;;----------------------------------------------------------------------------
;; change-pros: changes the pronouns of the sentence so that Eliza can
;; come back with the appropriately switched first and second person
;; references.

( defun change-pros ( sentence )
  ( cond
    ( ( null sentence ) nil )
    ( ( equal ( car sentence ) 'you )
      ( cons 'I ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'I )
      ( cons 'you ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'myself )
      ( cons 'yourself ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'yourself )
      ( cons 'myself ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'my )
      ( cons 'your ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'your )
      ( cons 'my ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) '(I am) )
      ( cons '(you are) ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) '(you are) )
      ( cons '(I am) ( change-pros ( cdr sentence ) ) ) )

    ;; CHANGE THIS: add more cases here of pronouns or other words
    ;; that should flip in order for this to work well

    ( t ( cons ( car sentence ) ( change-pros ( cdr sentence ) ) ) ) ) )

;;----------------------------------------------------------------------------
;; respond: given a sentence, looks through the database in search of
;; a matching pattern and the response; given the database response,
;; uses 'instantiate' to fill in the blanks, and returns the completed
;; response

;(defun len (list)
;  (print list)
;  (if list
;     (print (+ 1 (len (cdr list))))
;     0))

( defun respond ( sentence db )
  ( cond
    ;; end of DB, return nil - should never really happen
    ( ( null db ) nil )

    ;; if the result of matching the sentence against the current
    ;; pattern is a success, produce this response
    ( ( success ( setq result ( match sentence ( first ( car db ) ) ) ) )
        (len db)
	(print (list-length db))
	(setf ran_num(random (list-length db)))
	(print ran_num)
     	( instantiate result (second(nth ran_num db ) )))

    ;; otherwise, keep looking through the DB
    ( t ( respond sentence ( cdr db ) ) ) ) )

;;----------------------------------------------------------------------------
;; match: if there is not a match between this pattern and this data,
;; returns 'fail;' otherwise, returns the sentence in partitioned
;; format

( defun match ( data pattern )
  ( cond
    ;; end of both data and pattern; a match
    ( ( and ( null data ) ( null pattern ) ) nil )

    ;; end of pattern, but not end of data; no match
    ( ( null pattern ) fail )

    ;; end of data, but not end of pattern; if the pattern starts with
    ;; a variable, eat it and try and match the rest of the pattern to
    ;; the null sentence (will only work if all variables); otherwise,
    ;; fail
    ( ( null data )
      ( cond
	( ( variablep ( car pattern ) )
	  ( if ( success ( setq result ( match data ( cdr pattern ) ) ) )
	      result
	    fail ) )
	( t fail ) ) )


    ;; first item of data and pattern are identical; if the rest of it
    ;; matched, return the first item cons'ed with the rest of the
    ;; partitioned sentence; otherwise, fail
    ( ( equal ( car data ) ( car pattern ) )
      ( if ( success ( setq result ( match ( cdr data ) ( cdr pattern ) ) ) )
	  ( cons ( list ( car data ) ) result )
	fail ) )

    ;; first item of pattern is a variable; if the rest of the data
    ;; (minus the first word, matched to the variable) is a match with
    ;; all of the pattern, return the appropriate stuff; if all of the
    ;; data (variable eats nothing) matches the rest of the pattern,
    ;; return appropriate stuff; else, fail.
    ( ( variablep ( car pattern ) )
      ( cond
	;; variable eats nothing;  () is put in partitioned sentence
	( ( success ( setq result ( match data ( cdr pattern ) ) ) )
	  ( cons () result ) )
	;; variable eats one word; word is cons'ed into the first
	;; element of the partitioned sentence, assuming that the step
	;; before an actual match word would be a ()
	( ( success ( setq result ( match ( cdr data ) pattern ) ) )
	  ( cons ( cons ( car data ) ( car result ) ) ( cdr result ) ) )
	;; otherwise, fail
	( t fail ) ) )

    ( t fail ) ) )

;;----------------------------------------------------------------------------
;; instantiate: takes a partitioned sentence and the response it has
;; been matched to and generates the appropriated completed response

( defun instantiate ( partitioned response )
  ( cond
    ( ( null response ) nil )
    ;; numbers indicate what part of the partitioned sentence to
    ;; insert into the response
    ( ( numberp ( car response ) )
      (print car response)
      ( setq index ( - ( car response ) 1 ) )
      (print index)
      ( append ( nth index partitioned )
	     ( instantiate partitioned ( cdr response ) ) ) )
    ( t ( cons ( car response )
	     ( instantiate partitioned ( cdr response ) ) ) ) ) )

;;---------------------------------------------------------------------------
;;
;;  			     helping functions
;;
;;---------------------------------------------------------------------------

( setq fail '-1 )

( defun success ( result )
  ( not ( equal result fail ) ) )

( defun variablep ( word )
  ( equal word '0 ) )


;;---------------------------------------------------------------------------
;;
;;  			         database
;;
;;---------------------------------------------------------------------------

;; CHANGE THIS: add more to this database so that the interaction is
;; more interesting and communicative and so that Eliza sounds like you
;; would sound in the same conversation!
;;---------------------------------------------------------------------------

( setq database
       '(
   ;; keywords
   ( (0 hurt 0)
     (You should see a doctor for that) )
   ( (0 smell 0)
     (I do not have a sense of smell thankfully) )
   ( (0 fun 0)
     (Fun is an understatment) )
   ( (0 life 0)
     (Just so you know the meaning of life is \42) )
   ( (0 curious 0)
     (They say curiousity killed the cat) )

	 ;; example greetings/farewells -- change them to sound like you
	 ( (Hello 0)
	   (Hey hey!) )
	 ( (0 Goodbye 0)
	   (Adios!) )

   ;; example questions
   ( (0 you came here because 0)
     (1 Morphius told you to!) )
   ( (0 I came here because 0)
     (Because I live here and you are trespassing......) )
   ( (0 How has 0)
     (I do not how 4))

   ;; feelings
   ( (0 you think 0)
     (And just why do you think 4 ?) )
   ( (0 you are feeling 0)
     (Why are you feeling 4 ?) )

   ;; the catch-alls
   ( (0)
     (Could you expand on that?) )
   (  (0)
     (Tell me more))
   ( (0)
    (You lost me....))
   ( (0)
    (Run that back))
   ( (0)
    (You don\'t say....))
   ( (0)
    (You don\'t say....))
    ) )
