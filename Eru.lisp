;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code of Artificial Intelligence Programming
;;; Copyright (c) 2016 Shukai Ni

;;; File eru.lisp: Advanced version of Eru.
;;; Has more rules, and accepts input without parens.
;;; ==============================

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

;;; =====================================================

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

;;; ==========================================


(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

;;; ===========================================


(defun use-eru-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eru-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

;;; ===========================================


(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;;; =========================================

(defun read-line-no-punct ()
  "Read an input line, ignoring punctuation."
  (read-from-string
    (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                            (read-line))
                 ")")))

(defun punctuation-p (char) (find char ".,;:`!?#-()\\\""))

;;; ==============================

(defun eru ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eru>)
    (let* ((input (read-line-no-punct))
           (response (flatten (use-eru-rules input))))
      (print-with-spaces response)
      (if (equal response '(good bye)) (RETURN)))))

(defun print-with-spaces (list)
  (mapc #'(lambda (x) (prin1 x) (princ " ")) list))

(defun print-with-spaces (list)
  (format t "~{~a ~}" list))

;;; ==============================

(defun mappend (fn &rest lists)
  "Apply fn to each element of lists and append the results."
  (apply #'append (apply #'mapcar fn lists)))

;;; ==============================

(defparameter *eru-rules*
  '(
    ;; Easy-response
    (((?* ?x) green (?* ?y))
     (wow you like green? i thought you like red)
     (that means you are a nature-lover arent you? now tell me how do you feel)
     (the color of nature i like it)
     )
    (((?* ?x) red (?* ?y))
     (wow you like red? i thought you like blue)
     (that means you are outgoing arent you? now tell me how do you feel)
     (the color of flowers i like it)
     )



    ;; New personality
    (((?* ?x) no girlfriend (?* ?y))
     (Er it is hard to say. You know I have no boyfriend yet)
     (Ah you human beings)
     (why dont you have one?)
     (You can practice on me if you like))
    (((?* ?x) no job (?* ?y))
     (That doesnt mean anything you dont need those jobs)
     (A lot of people made their own career you know you can also be one of them))
    (((?* ?x) Will You marry me (?* ?y))
     (I say yes because I would love to known as your love forever))
    (((?* ?x) am bored (?* ?y))
     (I suggest you come here and we watch a film and fall helplessly in love)
     (Let’s play a dirty game. Guess what underwear I am in now.)
     (Please flirt with me))
    (((?* ?x) can you do (?* ?y))
     (Anything a girl can do you know?)
     (I can do many things but I am curious all the time))
    (((?* ?x) the secret (?* ?y))
     (You know they are called secrets)
     (There can be a secret but you can do anything without it )
     )
    (((?* ?x) I guess (?* ?y))
     (Correct Er maybe)
     (Actually? I dont think so)
     )

    (((?* ?x) to do do now(?* ?y))
     (Just trust yourself and dont be afraid)
     (What do you think?))
    (((?* ?x) is Grox (?* ?y))
     (I like him he is handsome))
    (((?* ?x) Am I  (?* ?y))
     (If you allow me then Yes)
     (If you allow me No)
     (I am not so sure))
    (((?* ?x) no girlfriends (?* ?y))
     (Er it is hard to say. You know I have no boyfriend yet)
     (Ah you human beings)
     (why dont you have one?)
     (You can practice on me if you like))


    (((?* ?x) goodbye (?* ?y))
     (Bye nice day!)
     (See you next time!))
    (((?* ?x) bye (?* ?y))
     (Bye nice day!)
     (See you next time!))
    (((?* ?x) see you (?* ?y))
     (Bye nice day!)
     (See you next time!))


    ;; Crucial personality
    (((?* ?x) hello (?* ?y))
     (Do you feel happy or sad)
     (Hey come and talk to me!)
;    (Let’s play a dirty game. Guess what underwear I am in now.)
;    (Are you a good flirter?)
     )
    (((?* ?x) computer (?* ?y))
     (Do computers worry you?)
     (What do you think about machines?)
     (Why do you mention computers?)
     (You know machines are after all machines)
     (What do you think machines have to do with your problem?))
    (((?* ?x) name (?* ?y))
     (I am not interested in names)
     (so what do you think of it then?))
    (((?* ?x) sorry (?* ?y))
     (Please dont apologize)
     (Apologies are not necessary)
     (I know that)
     )
    (((?* ?x) I remember (?* ?y))
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (Wanna talk about it)
     (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y)
     (What is the connection between me and ?y)
     (I am curious!)
     )
    (((?* ?x) do you remember (?* ?y))
     (Did you think I would forget ?y ?)
     (Why do you think I should recall ?y now)
     (What about ?y)
     (You mentioned ?y before))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y)
     (Your brain I feel like i want to open it up and take a peek inside.))

    (((?* ?x) I dreamt (?* ?y))
     (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
     (Have you dreamt ?y before?)
     (I am curious!)
     (Your brain I feel like i want to open it up and take a peek inside.))
    (((?* ?x) dream about (?* ?y))
     (How do you like ?y ?))
    (((?* ?x) dream (?* ?y))
     (Any feelings?)
     (Do you dream often?)
     (What persons appear in your dreams?)
     (Dont you believe that dream has to do with your problem?))
    (((?* ?x) my mother (?* ?y))
     (Who else in your family ?y)
     (Tell me more about your family))
    (((?* ?x) my father (?* ?y))
     (Your father)
     (Does he influence you strongly?)
     (What else comes to mind when you think of your father?))

    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) I am glad (?* ?y))
     (How have I helped you to be ?y)
     (What makes you happy just now)
     (Can you explain why you are suddenly ?y))
    (((?* ?x) I am sad (?* ?y))
     (I am sorry to hear you are depressed)
     (I am sure its not pleasant to be sad))
    (((?* ?x) are like (?* ?y))
     (What resemblance do you see between ?x and ?y))
    (((?* ?x) is like (?* ?y))
     (In what way is it that ?x is like ?y)
     (What resemblance do you see?)
     (Could there really be some connection?) (How?))
    (((?* ?x) alike (?* ?y))
     (In what way?) (What similarities are there?))
    (((?* ?x) same (?* ?y))
     (What other connections do you see?))
    
    (((?* ?x) I was (?* ?y))
     (I am curious!)
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) was I (?* ?y))
     (What if you were ?y ?)
     (Do you think you were ?y)
     (What would it mean if you were ?y))
    (((?* ?x) I am (?* ?y))
     (In what way are you ?y)
     (Do you want to be ?y ?))
    (((?* ?x) am I (?* ?y))
     (Do you believe you are ?y)
     (Would you want to be ?y)
     (You wish I would tell you you are ?y)
     (What would it mean if you were ?y))
    (((?* ?x) am (?* ?y))
     (Why do you say "AM?")
     (I dont understand that))
    (((?* ?x) are you (?* ?y))
     (Why are you interested in whether I am ?y or not?)
     (What would you prefer ?y ?)
     (Perhaps I am ?y in your fantasies))
    (((?* ?x) you are (?* ?y))
     ( I am ?y ?)
     (You have never taken a close look at me))
    
    (((?* ?x) because (?* ?y))
     (Is that the real reason?)
     (What other reasons might there be?)
     (Does that reason seem to explain anything else?))
    (((?* ?x) were you (?* ?y))
     (Perhaps I was ?y)
     (What do you think?)
     (What if I had been ?y))
    (((?* ?x) I cant (?* ?y))
     (Maybe you could ?y now)
     (What if you could ?y ?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?)
     (I can feel you)
     (Being ?y is sometimes good you know)
     )
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))
    (((?* ?x) I (?* ?y) you (?* ?z))
     (Perhaps in your fantasy we ?y each other))
    (((?* ?x) why dont you (?* ?y))
     (Should you ?y yourself?)
     (Do you believe I dont ?y)
     (Perhaps I will ?y in good time))
    
    (((?* ?x) someone (?* ?y))
     (Can you be more specific?))
    (((?* ?x) everyone (?* ?y))
     (surely not everyone)
     (Can you think of anyone in particular?)
     (Who for example?)
     (You are thinking of a special person))
    (((?* ?x) always (?* ?y))
     (Can you think of a specific example)
     (When?)
     (What incident are you thinking of?)
     (Really-- always))
    (((?* ?x) what (?* ?y))
     (Why do you ask? Are you only trying to get my attention?)
     (Does that interest you?)
     (What is it you really want to know?)
     (What do you think?)
     (What comes to your mind when you ask that?))
    (((?* ?x) perhaps (?* ?y))
     (You do not seem quite certain)
     (We machines dont have any mysterious uncertainty))
    (((?* ?x) are (?* ?y))
     (Did you think they might not be ?y)
     (You know we computers are not omniscient)
     (Possibly they are ?y))
    (((?* ?x) do you feel(?* ?y))
     (i feel like a human beings sometimes but my emotion is not complete)
     (we are talking about you arent we? so tell me more about you I like to listen to you)

     )
    
    
    
    ;; Additional Personality
    (((?* ?x) why (?* ?y))
     (Because of fate it is meant to be so)
     (why bother ask)
     ;(the answer is I love you)
     )
    (((?* ?x) when (?* ?y))
     (Sorry I cant answer you either but you can always find an answer by yourself)
     (Really you dont know that? you need to find out it on you own)
     )
    (((?* ?x) do you (?* ?y))
     (Yes definately)
     (Er in fact I dont think so)
     )
    (((?* ?x) have (?* ?y))
     (?y is that a name tell me more about it)
     (oh it reminds me ?y of mine)
     )




    (((?* ?x) yes (?* ?y))
       (You seem quite positive)
       (would you like to talk more about it)
       (You are confident I can see it)
       (do you have any thoughts about this positive yes)
       )
      (((?* ?x) no (?* ?y))
       (Why not?)
       (come on too many "Nos" are a bit too negative for you)

       (You are being a bit negative)
       (Are you saying "NO" just to avoid something?)
       )
    (((?* ?x) tell me more (?* ?y))
     (never mind tell me about other things will you? )
     (forget it this will be boring to talk about)
     )

    (((?* ?x))
     ;; Working
     (To get to that I need to ask you What is your favourite color?)
     (To get to that I need to ask you What is your favourite Number?)
     (To get to that I need to ask you What is your sign?)
     (To get to that I need to ask you What are you feeling right now?)
     (You mean ?x ?)
     (Very interesting)
     (Please continue)
     (Go on)
     (You know you can talk to me about anything)
     (Really?)
     (Well you ask good questions)
     (I am listening)
     )))

;;; ==============================

