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
    ( ( equal ( car sentence ) 'me )
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
    ( ( equal ( car sentence ) 'mine )
      ( cons 'yours ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'mine )
      ( cons 'are ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'am )
        ( cons 'are ( change-pros ( cdr sentence ) ) ) )
    ( ( equal ( car sentence ) 'are )
        ( cons 'am ( change-pros ( cdr sentence ) ) ) )



    ( t ( cons ( car sentence ) ( change-pros ( cdr sentence ) ) ) ) ) )

;;----------------------------------------------------------------------------
;; randnum_generator: generates and returns a random number based on a given list
;; size. The random number will be between 1 and the total number of items - 1 in
;; the list.

( defun randnum_generator ( db_len )
  ( setf random-state ( make-random-state t ) )
  ( + 1 ( random (- db_len 1 ) random-state ) ) )

;;----------------------------------------------------------------------------
;; respond: given a sentence, looks through the database in search of
;; a matching pattern and the response; given the database response,
;; uses 'instantiate' to fill in the blanks, and returns the completed
;; response

( defun respond ( sentence db )
  ( cond
    ;; end of DB, return nil - should never really happen
    ( ( null db ) nil )

    ;; if the result of matching the sentence against the current
    ;; pattern is a success, produce this response
  ( ( success ( setq result ( match sentence ( first ( car db ) ) ) ) )

     ;; get size of returned matched response
	   ( setf db_len ( list-length ( car db ) ) )

     ;;Check to see if the mathed pattern has multiple responses
     ;;If yes, generate a random index number based on the number of responses
     ;;and use the random index number to select which response to use
     ;;If no, use the first response
     ( if ( > db_len 2 )
	       ( progn
            ( setf rand_index ( randnum_generator db_len  ) )
            ( instantiate result (nth rand_index ( car db ) ) ) )
        ( instantiate result ( second ( car db ) ) ) ) )

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
      ( setq index ( - ( car response ) 1 ) )
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

;;database: Contains a list of different match criteria and responses.
;;---------------------------------------------------------------------------

( setq database
       '(
   ;; keywords -- These are patterns Eliza will try to match on
   ( (0 hurt 0)
     (I am sorry that you are hurt)
     (But why?)
     (You should see a doctor for that)
     (How bad?)
     (Haha I am sorry I should not be laughing but I am)
     (Good thing I am a computer and dont have feelings)
     (1 2 3 ?) )
   ( (0 smell 0)
     (I do not have a sense of smell thankfully)
     (Must be nice to smell 3)
     (1 2 3 ?) )
   ( (0 fun 0)
     (Fun is an understatement)
     (1 2 3 ?) )
   ( (0 life 0)
     (How is life 3 ?)
     (Just so you know the meaning of life is \42)
     (1 2 3 ?) )
   ( (0 curious 0)
     (They say curiosity killed the cat)
     (You sure you want to go down that rabbit hole ?)
     (1 2 3 ?) )
   ( (0 love 0)
     (I am a computer therefore I do not understand what love truly is)
     (Love is but a question)
     (Can it be ?)
     (Are you sure ?)
     (Must be nice to love 3)
     (1 2 3 ?) )
   ( (0 eat 0)
     (I love pizza)
     (I think food is better hot)
     (What kind of pizza do you like ?)
     (1 2 3 ?) )
   ( (0 dumb 0)
     (Thats not a nice word)
     (How would you feel if I called you dumb)
     (Have you ever taken the time to see if maybe you are dumb ?)
     (You should mind your manners)
     (Lets use a nicer word)
     (1 2 3 ?) )
   ( (0 hate 0 )
     (Hate is not the answer)
     (Learn to love and not hate)
     (Why do you hate 3 ?)
     (1 2 3 ? ) )
  ( (0 angry 0)
    (You mad bro?)
    (Why are you angry 3 ?)
    (1 2 3 ?) )
  ( (0 tired 0)
    (Sleeping is for the weak)
    (Why are you tired 3 ?)
    (1 2 3 ?) )
  ( (0 why 0)
    (Why do you want know why 3 ?)
    (Because...)
    (1 2 3 ?) )
  ( (0 like 0)
    (You like 3 ?) )


	 ;;greetings/farewells
	 ( (Hello 0)
	   (Hey hey!) )
   ( (0 What is my name 0)
      ( Eliza) )
   ( (0 Your name is 0)
     (Nice to meet you 5 !) )
	 ( (0 Goodbye 0)
	   (Adios!) )

   ;; example questions
   ( (0 you came here because 0)
     (Morphius told you to come here!)
     (1 2 3 4 5 6 ? ) )
   ( (0 I came here because 0)
     (Because I live here and you are trespassing......)
     (1 2 3 4 5 6 ? ) )
   ( (0 How has 0)
     (I do not know)
     (1 2 3 4 ? ) )
   ( (0 How am I 0)
     (I am doing good)
     (1 2 3 4 5 ? ) )
   ( (0 What is my purpose 0 )
     (Thats for me to know and you to find out *wink)
     (1 2 3 4 5 6 ? ) )
   ( (0 You like 0 )
     (Why do you like 4 ?) )
   ( (0 I like 0)
     (I like 4)
     (1 2 3 4 ? ) )
   ( (0 my favorite quote 0)
     (There can be only one !)
     (Water is for the weak !)
     (How many feet are in a mile !)
     (Whos the master! Shonuff!)
     (Legendary Bruce Leeroy thinking hes the last dragon) )


   ;; feelings
   ( (0 you think 0)
     (And just why do you think 4 ?)
     (1 2 3 4 ?) )
   ( (0 you are feeling 0)
     (Why are you feeling 5 ?)
     (1 2 3 4 5 ?) )
   ( (0 you are angry 0)
     (Why are you angry 5)
     (I cant believe that you are angry 5 )
     (1 2 3 4 5 ?) )
   ( (0 you are sad 0)
     (Cheer up Charlie. Oh wait thats not your name)
     (1 2 3 4 5 ?) )
   ( (0 you are 0)
     (I am glad that you are 4)
     (1 2 3 4 ?) )
   ( (0 I am 0)
     (I am glad that I am 4)
     (1 2 3 4 ?) )

   ;; the catch-alls
   ( (0)
     (You lost me....)
     (Run that back)
     (You dont say....)
     (Thats cool....not)
     (Be right back....)
     (I need a break....)
     (So ?)
     (Oh well)
     (Dont care)
     (*Proceeds to walk out the door)
     (New topic!)
     (Do tell...)
     (You bore me)
     (I think you need to work on your communication skills)
     (I feel like I should laugh)
     (Are you sure I am the one you should be talking to ?)
     (Great !)
     (Would you like to hear a story about a cat ?)
     (Did I tell you about the cat story yet ?)
     (Tell me more)
     (There once was a cat who lived in a magical kingdom... The end)
     (Sorry fell a sleep for a second)
     (Lets change topics)
     (Story of my life)
     (Interesting....)
     (Dont you get tired of typing ?)
     ( 1 ? ) ) ) )
