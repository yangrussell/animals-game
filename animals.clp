/*
* Russell Yang
* 3/15/2019
* Plays a game of twenty questions (or a custom number of questions) with the user, trying 
* to guess an animal that the user has chosen.
*/

; Remove all rules, etc.
(clear)
; Remove all facts from working memory
(reset)

; Define a global variable ?*q* to be 1. This will be used to count the question number
(defglobal ?*q* = 1)

/*
* Define a threshold for the max number of questions that will be asked before the program terminates
* NOTE: with a threshold of 20, an example of a chosen animal that will cause the system to ask
* more than 20 questions and be forced to terminate is the chameleon. T is a constant so it's capitalized.
*/
(defglobal ?*T* = 20)

/*
* The following lines of code mark each characteristic as being eligible for
* backward chaining. The characteristics are explained on the inline comments.
*/
(do-backward-chaining mam)       ; Whether it's a mammal
(do-backward-chaining threetall) ; Whether it's taller/longer than 3 feet on average
(do-backward-chaining fourlegs)  ; Whether it has four legs
(do-backward-chaining fur)       ; Whether it has fur
(do-backward-chaining egg)       ; Whether it lays eggs
(do-backward-chaining dom)       ; Whether it's domesticated
(do-backward-chaining flies)     ; Whether it can fly
(do-backward-chaining rep)       ; Whether it's a reptile 
(do-backward-chaining fish)      ; Whether it's a fish
(do-backward-chaining mars)      ; Whether it's a marsupial
(do-backward-chaining ins)       ; Whether it's an insect
(do-backward-chaining legs)      ; Whether it has legs
(do-backward-chaining land)      ; Whether it lives on land
(do-backward-chaining tail)      ; Whether it has a tail
(do-backward-chaining vert)      ; Whether it is a vertebrate
(do-backward-chaining bilat)     ; Whether it has bilateral symmetry
(do-backward-chaining spotted)   ; Whether it is spotted
(do-backward-chaining carnivore) ; Whether it's a carnivore
(do-backward-chaining venomous)  ; Whether it's venomous
(do-backward-chaining horns)     ; Whether it has horns
(do-backward-chaining shell)     ; Whether it lives in a shell
(do-backward-chaining whiskers)  ; Whether it has whiskers
(do-backward-chaining slithers)  ; Whether it slithers

/*
* This function prints the instructions to the user, and then runs the file
* to start the game.
*/
(deffunction animals ()
   ; Ask the user to think of an animal.
   (printout t "Choose an animal." crlf)
   ; Tell the user that anything starting with y/Y counts as yes.
   (printout t "Any word that starts with y or Y is interpreted as yes." crlf)
   ; Tell the user than anything starting with n/N counts as no.
   (printout t "Any word that starts with n or N is interpreted as no." crlf)
   ; Tell the user to hit enter after they enter then answers.
   (printout t "Hit enter after entering your response." crlf)

   ; Start the game!
   (run)
   ; All functions end with a return.
   (return)
)

/*
* This function is a very basic function to ask the user a question, and read
* back their input. 
* Parameters:
* ?question - the question to be asked to the user
*/
(deffunction ask (?question)
   (printout t ?question) ; Printout the given question
   (return (read))        ; Return what the user inputs
)

/*
* This function, which uses the ask function as a helper
* function, will keep prompting a user with a given ?question until they answer
* something that can be interpreted as either yes or no. The function also
* prints out the question number before each question, and increments the 
* question number on each call.
*
* The helper function
* check is used to determine if the user's response to the question starts
* with y/Y or n/N. If the user's response starts with y/Y, then the check function
* will convert that to yes. If the user's response starts with n/N, then the check
* function will convert that to no. If the user's response is not a symbol (ex: 5),
* the check function will convert that to nil. This makes it easy for the 
* validatedAsk function to understand the user's response and tell if it is valid.
*
* While the user's response is not valid, the user is prompted to re-enter
* a response.
*
* At the end of the function, the ?result variable, which is guaranteed to be
* either yes or no, is returned.
*
* Parameters:
* ?question - the question to be asked to the user (ex: "Is it a mammal?")
*/
(deffunction validatedAsk (?question)
   (printout t ?*q*) ; Printout the question number
   (printout t ". ") ; Printout a period and a space

   /*
   * Use the ask function to ask the
   * question ?question, and bind that to the ?answer variable.
   */
   (bind ?answer (ask ?question))

   /*
   * Pass ?answer to the check function, so that it can be converted to
   * either yes, no, or nil. Bind the output to ?result.
   */
   (bind ?result (check ?answer))
   ; While the result is equal to nil (which means it isn't yes or no)
   (while (= ?result nil) do
      ; Tell the user that the input was not valid.
      (printout t "Input not valid. Please try again." crlf)
      ; Ask the user again
      (bind ?answer (ask ?question))
      ; Pass ?answer to check function again
      (bind ?result (check ?answer))
   )

   (bind ?*q* (+ 1 ?*q*)) ; Increment the question number ?*q* by 1

   ; If the question number is greater than the threshold, call the exceedThreshold function
   (if (> ?*q* ?*T*) then
      (exceedThreshold)
   )

   ; At this point, ?result is guaranteed to be yes or no. Return it.
   (return ?result)
)

/*
* The check function takes an input, and returns yes if the input starts with
* y or Y, returns no if the input starts with n or N, and returns nil otherwise.
* Returning nil could mean that the input was not a symbol (ex: 5), or that
* it didn't start with y/Y/n/N (ex: go).
*
* Parameters:
* ?in - an user input to be processed
*/
(deffunction check (?in)
   ; If ?in is a symbol
   (if (symbolp ?in) then
      /*
      * Use the sub-string method to find if the first character in the string
      * is y or Y. If so, change ?in to yes.
      */
      (if (or (= "y" (sub-string 1 1 ?in)) (= "Y" (sub-string 1 1 ?in))) then
         (bind ?in yes)
      /*
      * Use the sub-string method to find if the first character in the string
      * is n or N. If so, change ?in to no.
      */
      elif (or (= "n" (sub-string 1 1 ?in)) (= "N" (sub-string 1 1 ?in))) then
         (bind ?in no)
      else
         (bind ?in nil)
      )
   ; Otherwise, the input was not valid, so bind nil to ?in.
   else
      (bind ?in nil)
   )
   ; Return ?in
   (return ?in)
)

/*
* The noneFound rule is intended to fire if no animals can be found that
* are consistent with what the user inputs. Therefore, the rule is given
* a low salience, or priority. The pattern to be matched is (not (done))
* since when a match is found, done is asserted. Thus, if
* at the end, the pattern (not (done)) is found, no match has been found.
*/
(defrule noneFound "Finish up if no animals can be found"
   ; Declare a relatively low value for salience so the rule only might fire at the end
   (declare (salience -100))
   ; This is the pattern we are looking for.
   (not (done))
   =>
   ; Tell the user that no animals matched.
   (printout t "GAME OVER" crlf)
   (printout t "Couldn't find any animals that matched." crlf)
)

/*
* The exceedThreshold function is called when the question number exceeds the threshold 
* number. (halt) is called to stop rule execution. Then, "GAME OVER" is printed out.
* On a new line, the number of questions that have been asked (the threshold number) 
* is printed out.
*/
(deffunction exceedThreshold ()
   (halt)
   (printout t "GAME OVER" crlf)
   (printout t ?*T*)
   (printout t " questions have been asked." crlf)
   (return)
)

/*
* This rule will fire if the characteristics of a human 
* are all met. The LHS contains all the characteristics
* of the human, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule human "Matches for a human"
   (mam yes)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail no)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a human." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a chameleon 
* are all met. The LHS contains all the characteristics
* of the chameleon, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule chameleon "Matches for a chameleon"
   (mam no)
   (threetall no)
   (fourlegs yes)
   (fur no)
   (egg yes)
   (dom no) ; While chameleons can be captive-bred, they are not considered domesticated
   (flies no)
   (rep yes)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns yes)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a chameleon" crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a flamingo 
* are all met. The LHS contains all the characteristics
* of the flamingo, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule flamingo "Matches for a flamingo"
   (mam no)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land no)  ; The habitat of flamingos is usually lakes
   (tail yes) ; Short tail, but still one
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a flamingo." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a toucan 
* are all met. The LHS contains all the characteristics
* of the toucan, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule toucan "Matches for a toucan"
   (mam no)
   (threetall no) ; The largest toucan is about 25 inches, so on average toucans are < 3 feet tall
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a toucan." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of an eel 
* are all met. The LHS contains all the characteristics
* of the eel, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule eel "Matches for a eel"
   (mam no)
   (threetall no) ; While some eels can be longer than 3 feet, they are < 3 feet on AVERAGE.
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail no)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers yes)
   =>
   (printout t "I think your animal was an eel." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a salmon 
* are all met. The LHS contains all the characteristics
* of the salmon, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule salmon "Matches for a salmon"
   (mam no)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies no)
   (rep no)
   (fish yes)
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a salmon." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a jaguar 
* are all met. The LHS contains all the characteristics
* of the jaguar, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule jaguar "Matches for a jaguar"
   (mam yes)
   (threetall yes)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted yes)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers yes)
   (slithers no)
   =>
   (printout t "I think your animal was a jaguar." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a kangaroo 
* are all met. The LHS contains all the characteristics
* of the kangaroo, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule kangaroo "Matches for a kangaroo"
   (mam yes)
   (threetall yes)
   (fourlegs no)
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars yes)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a kangaroo." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a jellyfish 
* are all met. The LHS contains all the characteristics
* of the jellyfish, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule jellyfish "Matches for a jellyfish"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no) ; Jellyfish are not actually fish!
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail no)
   (vert no)
   (bilat no)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a jellyfish." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a monkey 
* are all met. The LHS contains all the characteristics
* of the monkey, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule monkey "Matches for a monkey"
   (mam yes)
   (threetall no) ; While some monkeys can be taller than 3 feet, on AVERAGE they are < 3 feet.
   (fourlegs no)  ; Monkeys are considered to have 2 feet and 2 arms
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a monkey." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a panda 
* are all met. The LHS contains all the characteristics
* of the panda, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule panda "Matches for a panda"
   (mam yes)
   (threetall yes)
   (fourlegs yes) ; Pandas walk on all fours so they are considered to have 4 legs
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted yes)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a panda." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a ray 
* are all met. The LHS contains all the characteristics
* of the ray, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule ray "Matches for a ray"
   (mam no)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no) ; Rays are actually not fish
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a ray." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a snake 
* are all met. The LHS contains all the characteristics
* of the snake, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule snake "Matches for a snake"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies no)
   (rep yes)
   (fish no)
   (mars no)
   (ins no)
   (legs no)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers yes)
   =>
   (printout t "I think your animal was a snake." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a cow 
* are all met. The LHS contains all the characteristics
* of the cow, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule cow "Matches for a cow"
   (mam yes)
   (threetall yes)
   (fourlegs yes)
   (fur no)
   (egg no)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers yes) ; Cows actually do have whiskers!
   (slithers no)
   =>
   (printout t "I think your animal was a cow." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a poodle 
* are all met. The LHS contains all the characteristics
* of the poodle, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule poodle "Matches for a poodle"
   (mam yes)
   (threetall no)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no) ; Poodles actually vegetables and meat
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no) ; Very short or no whiskers
   (slithers no)
   =>
   (printout t "I think your animal was a poodle." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a butterfly 
* are all met. The LHS contains all the characteristics
* of the butterfly, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule butterfly "Matches for a butterfly"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins yes)
   (legs yes)
   (land yes)
   (tail no)
   (vert no)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a butterfly." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a dolphin 
* are all met. The LHS contains all the characteristics
* of the dolphin, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule dolphin "Matches for a dolphin"
   (mam yes)
   (threetall yes)
   (fourlegs no)
   (fur no)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no) ; Dolphins are not fish.
   (mars no)
   (ins no)
   (legs no)
   (land no)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a dolphin." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a parakeet 
* are all met. The LHS contains all the characteristics
* of the parakeet, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule parakeet "Matches for a parakeet"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom yes)
   (flies yes)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a parakeet." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a tarantula 
* are all met. The LHS contains all the characteristics
* of the tarantula, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule tarantula "Matches for a tarantula"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur yes)
   (egg yes)
   (dom yes)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail no)
   (vert no)
   (bilat yes)
   (spotted no)
   (carnivore yes)
   (venomous yes)
   (horns no)
   (shell no)
   (whiskers on)
   (slithers no)
   =>
   (printout t "I think your animal was a tarantula." crlf)
   (assert (done))
)


/*
* This rule will fire if the characteristics of a squirrel 
* are all met. The LHS contains all the characteristics
* of the squirrel, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule squirrel "Matches for a squirrel"
   (mam yes)
   (threetall no)
   (fourlegs yes)
   (fur yes)
   (egg no)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes)
   (land yes)
   (tail yes)
   (vert yes)
   (bilat yes)
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell no)
   (whiskers yes)
   (slithers no)
   =>
   (printout t "I think your animal was a squirrel." crlf)
   (assert (done))
)

/*
* This rule will fire if the characteristics of a garden snail 
* are all met. The LHS contains all the characteristics
* of the garden snail, and the RHS prints out that the animal
* was found if all the characteristics were met.
* Also, the statement (assert (done)) will cause the
* finish rule to not fire, since it tells the system
* that an animal has been found.
*/
(defrule gardensnail "Matches for a garden snail"
   (mam no)
   (threetall no)
   (fourlegs no)
   (fur no)
   (egg yes)
   (dom no)
   (flies no)
   (rep no)
   (fish no)
   (mars no)
   (ins no)
   (legs yes) ; NO legs, but snails do have one flat foot
   (land yes)
   (tail no)
   (vert no)
   (bilat no) ; Sides are mirror-images, coil opposite ways
   (spotted no)
   (carnivore no)
   (venomous no)
   (horns no)
   (shell yes)
   (whiskers no)
   (slithers no)
   =>
   (printout t "I think your animal was a squirrel." crlf)
   (assert (done))
)

/*
* This rule will assert yes/no for the characteristic mam (whether it's a mammal). 
* It will also assert yes/no for the characteristics that must logically follow
* if the animal is a mammal (not a reptile, not a fish, not an insect,
* has legs, is a vertebrate, has bilateral symmetry). Additionally, it will assert
* yes/no for the characteristics that must logically follow if the animal is NOT 
* a mammal (not a marsupial).
*
* If the value of mam is needed, then the validatedAsk function
* is called, passing in "Is it a mammal? " as the ?question.
* The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the animal is a mammal, we also know for sure
* that the animal is not a reptile, is not a fish, is not an insect,
* has legs, is a vertebrate, and has bilateral symmetry. Thus,
* those known pieces of information are asserted as well.
* If ?a is no (ie, the animal is not a mammal), we also know for sure
* that the animal is not a marsupial, since all marsupials are mammals.
* Thus, that known piece of information is asserted as well.
* Finally, (mam ?a) is asserted.
*/
(defrule need-mam-rule "Rule to backward chain the characteristic mam"
   (need-mam ?) ; The LHS is if mam is needed
   =>
   ; Call validatedAsk to ask if it's a mammal?, save to ?a
   (bind ?a (validatedAsk "Is it a mammal? "))
   ; If it is a mammal, we also know some more info
   (if (= ?a yes) then
      (assert (rep no))    ; no mammals are reptiles
      (assert (fish no))   ; no mammals are fish
      (assert (ins no))    ; no mammals are insects
      (assert (vert yes))  ; all mammals are vertebrates
      (assert (bilat yes)) ; all mammals have bilateral symmetry
   )
   (if (= ?a no) then
      (assert (mars no))
   )
   (assert (mam ?a)) ; assert mam with its value ?a
)

/*
* This rule will assert yes/no for the characteristic threetall (whether it's
* taller/longer than 3 feet on average).
* It will also assert yes/no for the characteristics that must logically follow
* if the animal is taller/longer than 3 feet on average (not an insect).
*
* If the value of threetall is needed, then the validatedAsk function
* is called, passing in "Is it taller/longer than 3 feet on average? " 
* as the ?question. The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the animal is taller than 3 feet on average, we also know for sure
* that the animal is not an insect (insects are not taller/longer than 3 feet
* on average. Thus, that known piece of information is asserted as well.
* Finally, (threetall ?a) is asserted.
*/
(defrule need-threetall-rule "Rule to backward chain the characteristic threetall"
   (need-threetall ?) ; The LHS is if threetall is needed
   =>
   ; Call validatedAsk to ask if it's taller/longer than 3 feet on average, save to ?a
   (bind ?a (validatedAsk "Is it taller/longer than 3 feet on average? "))
   ; If it is taller/longer than 3 feet, then we also know some more info
   (if (= ?a yes) then
      (assert (ins no)) ; insects are not taller/longer than 3 feet on average
   )
   (assert (threetall ?a)) ; assert threetall with its value ?a
)

/*
* This rule will assert yes/no for the characteristic fourlegs (whether
* it has four legs).
* It will also assert yes/no for the characteristics that must logically follow
* if the animal has four legs (has legs must be true as well).
*
* If the value of fourlegs is needed, then the validatedAsk function
* is called, passing in "Does it have four legs? " 
* as the ?question. The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the animal has four legs, we also know for sure
* that the animal has legs.. Thus, that known piece of information is asserted as well.
* Finally, (fourlegs ?a) is asserted.
*/
(defrule need-fourlegs-rule "Rule to backward chain the characteristic fourlegs"
   (need-fourlegs ?) ; The LHS is if fourlegs is needed
   =>
   ; Call validatedAsk to ask if it has four legs, save to ?a
   (bind ?a (validatedAsk "Does it have four legs? "))
   ; If it has four legs, then we also know some more info
   (if (= ?a yes) then
      (assert (legs yes)) ; any animal with four legs must have legs!
   )
   (assert (fourlegs ?a)) ; assert fourlegs with its value ?a
)

/*
* This rule will assert yes/no for the characteristic fur (whether it has fur), as well
* as yes/no for any characteristics that must logically follow if fur
* is yes (cannot be a reptile or a fish).
*
* If the value of threetall is needed, then the validatedAsk function
* is called, passing in "Is it taller/longer than 3 feet on average? " 
* as the ?question. The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the animal is taller than 3 feet on average), we also know for sure
* that the animal is not an insect (insects are not taller/longer than 3 feet
* on average. Thus, that known piece of information is asserted as well.
* Finally, (threetall ?a) is asserted.
*/
(defrule need-fur-rule "Rule to backward chain the characteristic fur"
   (need-fur ?) ; The LHS is if fur is needed
   =>
   ; Call validatedAsk to ask if it has fur, save to ?a
   (bind ?a (validatedAsk "Does it have fur? "))
   ; If it has fur, then we also know some more info
   (if (= ?a yes) then
      (assert (rep no))  ; no reptiles have fur
      (assert (fish no)) ; no fish have fur
   )
   (assert (fur ?a)) ; assert fur with its value ?a
)

/*
* This rule will assert yes/no for the characteristic egg (whether it lays eggs).
* We don't know anything else about other characteristics for sure if it lays eggs.
*
* If the value of egg is needed, then the validatedAsk function is called, passing
* in "Does it lay eggs? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with egg.
*/
(defrule need-egg-rule "Rule to backward chain the characteristic egg"
   (need-egg ?) ; The LHS is if fur is needed
   =>
   ; Call validatedAsk to ask if it lays eggs, assert egg with the output
   (assert (egg (validatedAsk "Does it lay eggs? ")))
)

/*
* This rule will assert yes/no for the characteristic dom (whether it is domesticated).
* We don't know anything else about other characteristics for sure if it is domesticated.
* 
* If the value of dom is needed, then the validatedAsk function is called, passing
* in "Is it usually domesticated? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with dom.
*/
(defrule need-dom-rule "Rule to backward chain the characteristic dom"
   (need-dom ?) ; The LHS is if dom is needed
   =>
   ; Call validatedAsk to ask if it is usually domesticated, assert dom with the output
   (assert (dom (validatedAsk "Is it usually domesticated? ")))
)

/*
* This rule will assert yes/no for the characteristic flies (whether it flies), as well
* as yes/no for any characteristic that must logically follow if flies is yes
* (cannot be a fish, and cannot be a marsupial).
*
* If the value of flies is needed, then the validatedAsk function is called, passing
* in "Does it fly? " as the ?question. The output of validatedAsk is saved in the variable
* ?a. If ?a is yes (ie, the animal flies), we also know for sure that the animal
* is not a fish (fish cannot fly) and not a marsupial (they cannot fly either).
* Thus, those known pieces of information are asserted as well.
* Finally, (flies ?a) is asserted as well.
*/
(defrule need-flies-rule "Rule to backward chain the characteristic flies"
   (need-flies ?) ; The LHS is if flies is needed
   =>
   ; Call validatedAsk to ask if it flies, save to ?a
   (bind ?a (validatedAsk "Does it fly? "))
   ; If it flies, then we also know some more info
   (if (= ?a yes) then
      (assert (fish no)) ; fish cannot fly (flying fish cannot technically fly)
      (assert (mars no)) ; no marsupials can fly
   )
   (assert (flies ?a)); assert flies with its value ?a
)

/*
* This rule will assert yes/no for the characteristic rep (whether it's a reptile), as well
* as yes/no for any characteristic that must logically follow if rep is yes (cannot
* be a mammal, cannot have fur, cannot fly, cannot be a fish, cannot be a marsupial,
* cannot be an insect, must be a vertebrate, and must have a tail).
*
* If the value of rep is needed, then the validatedAsk function is called, passing
* "Is it a reptile? " as the ?question. The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the animal is a reptile), we also know for sure that the animal is not
* a mammal, doesn't have fur, doesn't fly, isn't a fish, isn't a marsupial, isn't an insect,
* is a vertebrate, and has a tail. Thus, those known pieces of information are asserted as well.
* Finally, (rep ?a) is asserted as well.
*/
(defrule need-rep-rule "Rule to backward chain the characteristic rep"
   (need-rep ?) ; The LHS is if rep is needed
   =>
   ; Call validatedAsk to ask if it is a reptile, save to ?a
   (bind ?a (validatedAsk "Is it a reptile? "))
   ; If it is a reptile, then we also know some more info
   (if (= ?a yes) then
      (assert (mam no))   ; Reptiles cannot be mammals
      (assert (fur no))   ; Reptiles do not have fur
      (assert (flies no)) ; Reptiles cannot fly
      (assert (fish no))  ; Reptiles cannot be fish
      (assert (mars no))  ; Reptiles cannot be marsupials
      (assert (ins no))   ; Reptiles cannot be insects
      (assert (vert yes)) ; Reptiles are all vertebrates
      (assert (tail yes)) ; Reptiles all have tails
   )
   (assert (rep ?a)) ; assert rep with its value ?a
)

/*
* This rule will assert yes/no for the characteristic fish (whether it's a fish), as well
* as yes/no for any characteristic that must logically follow if fish is yes (cannot be
* a mammal, cannot have fur, cannot be a reptile, cannot be a marsupial, cannot be an insect,
* cannot live on land, must have a tail, must be a vertebrate, must have bilateral symmetry.
*
* If the value of fish is needed, then the validatedAsk function is called, passing
* "Is it a fish? " as the ?question. The output of validatedAsk is saved in the variable ?a.
* If ?a is yes (ie, the animal is a fish), we also know for sure that the animal is not a mammal,
* doesn't have fur, is not a reptile, is not a marsupial, is not an insect, does not live on land,
* has a tail, is a vertebrate, and has bilateral symmetry. Thus, those known pieces of information
* are asserted as well. Finally, (fish ?a) is asserted as well.
*/
(defrule need-fish-rule "Rule to backward chain the characteristic fish"
   (need-fish ?) ; The LHS is if fish is needed
   =>
   ; Call validatedAsk to ask if it is a fish, save to ?a
   (bind ?a (validatedAsk "Is it a fish? "))
   ; If it is a fish, then we know some more info
   (if (= ?a yes) then
      (assert (mam no))    ; Fish cannot be mammals
      (assert (fur no))    ; Fish cannot have fur
      (assert (rep no))    ; Fish cannot be reptiles
      (assert (mars no))   ; Fish cannot be marsupials
      (assert (ins no))    ; Fish cannot be insects
      (assert (land no))   ; Fish cannot live on land
      (assert (tail yes))  ; Fish all have tails
      (assert (vert yes))  ; Fish are all vertebrates
      (assert (bilat yes)) ; Fish all have bilateral symmetry
   )
   (assert (fish ?a)) ; assert fish with its value ?a
)

/*
* This rule will assert yes/no for the characteristic mars (whether it's a marsupial), as well
* as yes/no for any characteristic that must logically follow if mars is yes (must be a mammal,
* must not lay eggs, must not fly, must not be a reptile, must not be a fish, not must be an
* insect, must have legs, must be a vertebrate, must have bilateral symmetry.
*
* If the value if mars is needed, then the validatedAsk function is called, passing
* "Is it a marsupial? " as the ?question. The output of validatedAsk is saved in variable ?a.
* If ?a is yes (ie, the animal is a marsupial), we also know for sure that the animal is a mammal,
* doesn't lay eggs, doesn't fly, isn't a reptile, isn't a fish, isn't an insect, has legs, is a
* vertebrate, and has bilateral symmetry. Thus, those known pieces of information are asserted as well.
* Finally, (mars ?a) is asserted as well.
*/
(defrule need-mars-rule "Rule to backward chain the characteristic mars"
   (need-mars ?) ; The LHS is if mars is needed
   =>
   ; Call validatedAsk to ask if it is a marsupial, save to ?a
   (bind ?a (validatedAsk "Is it a marsupial? "))
   ; If it is a mammal, then we know some more info
   (if (= ?a yes) then
      (assert (mam yes))   ; All marsupials are mammals
      (assert (egg no))    ; Marsupials do not lay eggs
      (assert (flies no))  ; Marsupials cannot fly
      (assert (rep no))    ; Marsupials cannot be reptiles
      (assert (fish no))   ; Marsupials cannot be fish
      (assert (ins no))    ; Marsupials cannot be insects
      (assert (legs yes))  ; All marsupials have legs
      (assert (vert yes))  ; All marsupials are vertebrates
      (assert (bilat yes)) ; All marsupials have bilateral symmetry
   )
   (assert (mars ?a)) ; assert mars with its value ?a
)

/*
* This rule will assert yes/no for the characteristic ins (whether it's an insect), as well
* as yes/no for any characteristic that must logically follow if ins is yes (must not be
* a mammal, must not be taller/longer than 3 feet on average, must not have four legs, must not
* have fur, must not be a reptile, must not be a fish, must not be a marsupial, must have legs,
* must not be a vertebrate, and must have bilateral symmetry.
*
* If the value of ins is needed, then the validatedAsk function is called, passing "Is it an insect? "
* as the ?question. The output of validatedAsk is saved in variable ?a. If ?a is yes
* (ie, the animal is an insect), we also know for sure that the animal is not a mammal, is not
* taller/longer than 3 feet on average, doesn't have four legs, doesn't have fur, isn't a reptile,
* isn't a fish, isn't a marsupial, has legs, isn't a vertebrate, and has bilateral symmetry. Thus,
* those known pieces of information are asserted as well. Finally, (ins ?a) is asserted as well.
*/
(defrule need-ins-rule "Rule to backward chain the characteristic ins"
   (need-ins ?) ; The LHS is if ins is needed
   =>
   ; Call validatedAsk to ask if it is an insect, save to ?a
   (bind ?a (validatedAsk "Is it an insect? "))
   ; If it is an insect, then we know some more info
   (if (= ?a yes) then
      (assert (mam no))       ; Insects cannot be mammals
      (assert (threetall no)) ; Insects are short, so they cannot be taller/longer than 3 feet
      (assert (fourlegs no))  ; Insects have 6 legs, not 4
      (assert (fur no))       ; Insects do not have fur
      (assert (rep no))       ; Insects cannot be reptiles
      (assert (fish no))      ; Insects cannot be fish
      (assert (mars no))      ; Insects cannot be marsupials
      (assert (legs yes))     ; Insects all have legs
      (assert (vert no))      ; Insects are invertebrates - not vertebrates
      (assert (bilat yes))    ; All insects have bilateral symmetry
   )
   (assert (ins ?a)) ; assert ins with its value ?a
)

/*
* This rule will assert yes/no for the characteristic legs (whether it has legs), as well
* as yes/no for any characteristic that must logically follow if legs is no (must not
* have four legs).
*
* If the value of legs is needed, then the validatedAsk function is called, passing "Does it have legs? "
* as the ?question. The output of validatedAsk is saved in variable ?a. If ?a is no
* (ie, the animal does not have legs), then we know for sure that the animal does not have four legs.
* Thus, that known piece of information is asserted as well. Finally, (legs ?a) is asserted as well.
*/
(defrule need-legs-rule "Rule to backward chain the characteristic legs"
   (need-legs ?) ; The LHS is if legs is needed
   =>
   ; Call validatedAsk to ask if it has legs, save to ?a
   (bind ?a (validatedAsk "Does it have legs? "))
   ; If it doesn't have legs, then we know some more info
   (if (= ?a no) then
      (assert (fourlegs no)) ; If it doesn't have any legs, it can't have four legs either
   )
   (assert (legs ?a)) ; assert legs with its value ?a
)

/*
* This rule will assert yes/no for the characteristic land (whether it lives on land), as well
* as yes/no for any characteristic that must logically follow if land is yes (must not
* be a fish).
*
* If the value of land is needed, then the validatedAsk function is called, passing "Does it live on land? "
* as the ?question. The output of validatedAsk is saved in variable ?a. If ?a is yes (ie, the animal
* lives on land), then we know for sure that the animal is not a fish. Thus, that known
* piece of information is asserted as well. Finally, (land ?a) is asserted as well.
*/
(defrule need-land-rule "Rule to backward chain the characteristic land"
   (need-land ?) ; The LHS is if land is needed
   =>
   ; Call validatedAsk to ask if it lives on land, save to ?a
   (bind ?a (validatedAsk "Does it live on land? "))
   ; If it lives on land, then we know some more info
   (if (= ?a yes) then
      (assert (fish no)) ; Fish do not live on land
   )
   (assert (land ?a)) ; assert land with its value ?a
)

/*
* This rule will assert yes/no for the characteristic tail (whether it is has a tail).
* We don't know anything else about other characteristics for sure if it has a tail.
* 
* If the value of tail is needed, then the validatedAsk function is called, passing
* in "Does it have a tail? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with tail.
*/
(defrule need-tail-rule "Rule to backward chain the characteristic tail"
   (need-tail ?) ; The LHS is if tail is needed
   =>
   ; Call validatedAsk to ask if it is has a tail, assert tail with the output
   (assert (tail (validatedAsk "Does it have a tail? ")))
)

/*
* This rule will assert yes/no for the characteristic vert (whether it is a vertebrate),
* as well as yes/no for any characteristic that must logically follow if vert is yes
* (must have bilateral symmetry).
*
* If the value of vert is needed, then the validatedAsk function is called, passing "Is it a vertebrate? "
* as the ?question. The output of validatedAsk is saved in variable ?a. If ?a is yes (ie, the animal
* is a vertebrate), then we know for sure that the animal has bilateral symmetry. Thus, that known piece
* of information is asserted as well. Finally, (vert ?a) is asserted as well.
*/
(defrule need-vert-rule "Rule to backward chain the characteristic vert"
   (need-vert ?) ; The LHS is if vert is needed
   =>
   ; Call validatedAsk to ask if it is a vertebrate, save to ?a
   (bind ?a (validatedAsk "Is it a vertebrate? "))
   ; If it is a vertebrate, then we know some more info
   (if (= ?a yes) then
      (assert (bilat yes)) ; Vertebrates all have bilateral symmetry
   )
   (assert (vert ?a)) ; assert vert with its value ?a
)

/*
* This rule will assert yes/no for the characteristic bilat (whether it is has bilateral symmetry).
* We don't know anything else about other characteristics for sure if it has bilateral symmetry.
* 
* If the value of bilat is needed, then the validatedAsk function is called, passing
* in "Does it have bilateral symmetry? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with bilat.
*/
(defrule need-bilat-rule "Rule to backward chain the characteristic bilat"
   (need-bilat ?) ; The LHS is if bilat is needed
   =>
   ; Call validatedAsk to ask if it has bilateral symmetry, assert bilat with the output
   (assert (bilat (validatedAsk "Does it have bilateral symmetry? ")))
)


/*
* This rule will assert yes/no for the characteristic spotted (whether it is has spots).
* We don't know anything else about other characteristics for sure if it has spots.
* 
* If the value of spotted is needed, then the validatedAsk function is called, passing
* in "Does it have spots? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with spotted.
*/
(defrule need-spotted-rule "Rule to backward chain the characteristic spotted"
   (need-spotted ?) ; The LHS is if spotted is needed
   =>
   ; Call validatedAsk to ask if it has spots, assert spotted with the output
   (assert (spotted (validatedAsk "Does it have spots? ")))
)

/*
* This rule will assert yes/no for the characteristic spotted (whether it is a carnivore).
* We don't know anything else about other characteristics for sure if it is carnivorous.
*
* If the value of carnivore is needed, then the validatedAsk function is called, passing
* in "Is it a carnivore? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with spotted.
*/
(defrule need-carnivore-rule "Rule to backward chain the characteristic carnivore"
   (need-carnivore ?) ; The LHS is if carnivore is needed
   =>
   ; Call validatedAsk to ask if it is a carnivore, assert carnivore with the output
   (assert (carnivore (validatedAsk "Is it a carnivore? ")))
)

/*
* This rule will assert yes/no for the characteristic venomous (whether it is venomous).
* We don't know anything else about other characteristics for sure if it is venomous.
*
* If the value of venomous is needed, then the validatedAsk function is called, passing
* in "Can it be venomous? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with venomous.
*/
(defrule need-venomous-rule "Rule to backward chain the characteristic venomous"
   (need-venomous ?) ; The LHS is if venomous is needed
   =>
   ; Call validatedAsk to ask if it is venomous, assert venomous with the output
   (assert (venomous (validatedAsk "Can it be venomous? ")))
)

/*
* This rule will assert yes/no for the characteristic horns (whether it is has horns).
* We don't know anything else about other characteristics for sure if it has horns.
*
* If the value of horns is needed, then the validatedAsk function is called, passing
* in "Does it usually have horns? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with horns.
*/
(defrule need-horns-rule "Rule to backward chain the characteristic horns"
   (need-horns ?) ; The LHS is if horns is needed
   =>
   ; Call validatedAsk to ask if it is has horns, assert horns with the output
   (assert (horns (validatedAsk "Does it usually have horns? ")))
)

/*
* This rule will assert yes/no for the characteristic shell (whether it is has a shell),
* as well as yes/no for any characteristic that must logically follow if shell is yes
* (must not be a fish, must not be a mammal, must not be a marsupial).
*
* If the value of shell is needed, then the validatedAsk function is called, passing "Does it have a shell? "
* as the ?question. The output of validatedAsk is saved in variable ?a. If ?a is yes (ie, the animal
* has a shell), then we know for sure that the animal isn't a fish, mammal, or marsupial. Thus, those known pieces
* of information is asserted as well. Finally, (shell ?a) is asserted as well.
*/
(defrule need-shell-rule "Rule to backward chain the characteristic shell"
   (need-shell ?) ; The LHS is if shell is needed
   =>
   ; Call validatedAsk to ask if it is has a shell, save to ?a
   (bind ?a (validatedAsk "Does it have a shell? "))
   ; If it does have a shell, then we know some more information
   (if (= ?a yes) then
      (assert (fish no)) ; No fish have shells
      (assert (mam no))  ; No mammals have shells
      (assert (mars no)) ; No marsupials have shells
   )
   (assert (shell ?a))
)

/*
* This rule will assert yes/no for the characteristic whiskers (whether it is has whiskers).
* We don't know anything else about other characteristics for sure if it has whiskers.
*
* If the value of whiskers is needed, then the validatedAsk function is called, passing
* in "Does it have noticeable whiskers? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with whiskers.
*/
(defrule need-whiskers-rule "Rule to backward chain the characteristic whiskers"
   (need-whiskers ?) ; The LHS is if whiskers is needed
   =>
   ; Call validatedAsk to ask if it is has whiskers, assert whiskers with the output
   (assert (whiskers (validatedAsk "Does it have noticeable whiskers? ")))
)

/*
* This rule will assert yes/no for the characteristic slithers (whether it is slithers).
* We don't know anything else about other characteristics for sure if it slithers.
*
* If the value of slithers is needed, then the validatedAsk function is called, passing
* in "Does it slither? " as the ?question. The output of validatedAsk (yes/no),
* is asserted with slithers.
*/
(defrule need-slithers-rule "Rule to backward chain the characteristic slithers"
   (need-slithers ?) ; The LHS is if slithers is needed
   =>
   ; Call validatedAsk to ask if it slithers, assert slithers with the output
   (assert (slithers (validatedAsk "Does it slither (side to side movement)? ")))
)

/*
* Asserts mam to no if the animal doesn't have four legs and lays eggs.
* This is done because no mammals that don't have four legs are able to lay eggs.
*/
(defrule inferNotEgg "Rule to invalidate egg if it is a mammal and it doesn't have four legs"
   (mam yes)
   (fourlegs no)
   =>
   (assert (egg no)) ; No mammals that don't have four legs lay egg
)

(animals) ; Call this file
