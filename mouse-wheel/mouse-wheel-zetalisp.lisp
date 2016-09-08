;;; -*- Syntax: Zetalisp; Mode: LISP; Package: SYSTEM-INTERNALS; BASE: 8 -*-

;; Correctly parse "mouse keys" #\Mouse-U and #\Mouse-D, changes in (COND (MOUSE
#||
;; SYSTEM-INTERNALS
;;; This function is given a symbol (or a string) which is expected to look
;;; like Control-Meta-A or Control-Meta-Abort or something.  It should return
;;; NIL if the print-name doesn't look like that, or the character object if
;;; it does.  (No longer true: It always returns a character in the standard character set.)
;;;--- Above comment is false, for example (XR-PARSE-KEYBOARD-CHAR "f-x") => NIL.
(DEFUN XR-PARSE-KEYBOARD-CHAR (STRING)
  (SETQ STRING (STRING STRING))
  (MACROLET
    ((XR-STR-CMP  (STRING)
       `(AND (= LEN ,(STRING-LENGTH STRING))
	     (%STRING-EQUAL ,STRING 0 STRING 1+PREV-HYPHEN-POS ,(STRING-LENGTH STRING))))
     (CONSTRUCT-CHAR (CHAR-CODE &OPTIONAL CHAR-BITS)
       `(CODE-CHAR ,CHAR-CODE ,@(WHEN CHAR-BITS (NCONS CHAR-BITS)))))
    (LET*
      ((END (ARRAY-ACTIVE-LENGTH STRING))
       (BITS (CODE-CHAR 0))
       (SHIFT NIL)
       (MOUSE NIL)
       TEM
       (CHAR (LOOP FOR START FIRST 0 THEN (1+ HYPHEN-POS)
		   FOR 1+PREV-HYPHEN-POS = 0 THEN (1+ HYPHEN-POS)
		   WHILE (< 1+PREV-HYPHEN-POS END)
		   FOR HYPHEN-POS = (OR (STRING-SEARCH-CHAR #/- STRING START END) END)
		   DO (LET ((LEN (- HYPHEN-POS 1+PREV-HYPHEN-POS)))
			(COND (MOUSE
			       (PUSH (SUBSTRING STRING 1+PREV-HYPHEN-POS HYPHEN-POS) MOUSE))
			      ((OR (XR-STR-CMP "CTRL")
				   (XR-STR-CMP "CONTROL"))
			       (SETF (CHAR-BIT BITS :CONTROL) T))
			      ((XR-STR-CMP "META")
			       (SETF (CHAR-BIT BITS :META) T))
			      ((XR-STR-CMP "HYPER")
			       (SETF (CHAR-BIT BITS :HYPER) T))
			      ((XR-STR-CMP "SUPER")
			       (SETF (CHAR-BIT BITS :SUPER) T))
			      ((OR (XR-STR-CMP "SHIFT")
				   (XR-STR-CMP "SH"))
			       (SETQ SHIFT T))
			      ((= 1+PREV-HYPHEN-POS (1- END))
			       ;; Single character. Just return it.
			       (RETURN (AREF STRING 1+PREV-HYPHEN-POS)))
			      ((= 1+PREV-HYPHEN-POS (1- HYPHEN-POS))
			       (IF (SETQ TEM
					 (CL:ASSOC (CHAR-UPCASE (STRIP-STYLE (AREF STRING 1+PREV-HYPHEN-POS)))
						   '((#/C . :CONTROL)
						     (#/M . :META)
						     (#/H . :HYPER)
						     (#/S . :SUPER))))
				   (SETF (CHAR-BIT BITS (CDR TEM)) T)
				   (RETURN NIL)))
			      ((XR-STR-CMP "MOUSE")
			       (PUSH "MOUSE" MOUSE))
			      ((SETQ TEM (CL:NAME-CHAR (SUBSTRING STRING
								  1+PREV-HYPHEN-POS END)))
			       (RETURN TEM))
			      (T (RETURN NIL)))))))
      ;; Now combine CHAR, BITS, and SHIFT into result.
      (COND (MOUSE
	     (SETF MOUSE (NREVERSE MOUSE))
	     (AND ( 2 (LENGTH MOUSE) 3)
		  (LET ((CLICKS (COND ((THIRD MOUSE)
				       (SELECTOR (THIRD MOUSE) STRING-EQUAL
					 ("1" 0)
					 ("2" 1)
					 (OTHERWISE
					   (RETURN-FROM XR-PARSE-KEYBOARD-CHAR NIL))))
				      (SHIFT 1)
				      (T 0)))
			(BUTTON (SELECTOR (SECOND MOUSE) STRING-EQUAL
				  (("L" "LEFT" "1") 0)
				  (("M" "MIDDLE" "2") 1)
				  (("R" "RIGHT" "3") 2)
				  (("U" "WheelUp" "4") 3)
				  (("D" "WheelDown" "5") 4)
				  (OTHERWISE (RETURN-FROM XR-PARSE-KEYBOARD-CHAR NIL)))))
		    (MAKE-MOUSE-CHAR BUTTON (+ (* CLICKS 20) (CHAR-BITS BITS))))))
	    ((NULL CHAR) NIL)			;Unrecognized character
	    ((ZEROP (CHAR-BITS BITS))		;Not controllified
	     (AND (NOT SHIFT) CHAR))
	    ((ALPHA-CHAR-P CHAR)		;Control-letter: adjust case
	     (CONSTRUCT-CHAR (IF SHIFT
				 (LOGIOR (CHAR-CODE CHAR) 40)
				 (LOGAND (CHAR-CODE CHAR) (LOGNOT 40)))
			     (CHAR-BITS BITS)))	
    ((NULL SHIFT) (CONSTRUCT-CHAR (CHAR-CODE CHAR) (CHAR-BITS BITS)))))))

(common-lisp-user::adjust-array *mouse-char-cache* (list 5 (* 2 char-bits-limit)))

(defun update-mouse-char-cache ()
  (loop with array = *mouse-char-cache*
	for button from 3 below 5
	do (loop for bits below (* 2 char-bits-limit)
		 do (setf (aref array button bits)
			  (make-mouse-char-internal button button bits bits)))))

(update-mouse-char-cache)

||#

;; Correctly format "mouse keys" #\Mouse-U and #\Mouse-D, changes in (IF LONG-NAME
;; FORMAT
;;; The same thing all over again for mouse "characters".
(DEFUN FORMAT-MOUSE-CHARACTER (STREAM CH LONG-BITS LONG-NAME IGNORE READABLE)
  (WHEN READABLE 
   (SEND STREAM :STRING-OUT "#\"))
  (LET* ((BITS (CHAR-MOUSE-BITS CH))
	 (BUTTON (CHAR-MOUSE-BUTTON CH)))
    ;; If bucky bits are present, print them
    (FORMAT-CHARACTER-BITS STREAM BITS LONG-BITS (BIT-TEST 1_4 BITS))
    
    ;; Print main part of character as a mouse character
    (SEND STREAM :STRING-OUT "Mouse-")
    (IF LONG-NAME
	(SEND STREAM :STRING-OUT (NTH BUTTON '("Left" "Middle" "Right" "WheelUp" "WheelDown")))
	(SEND STREAM :TYO (NTH BUTTON '(#\L #\M #\R #\U #\D))))))

