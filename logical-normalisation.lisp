(defpackage #:logical-normalisation
  (:use
   :cl
   :iterate
   :cl-ppcre
   :split-sequence))

(in-package #:logical-normalisation)

;; Aim of Program:
;; Formulate logical formula to DNF or CNF
;; True (1) and False (0)
;; Expressions to work with initially: of form A connective B
;; Initial connnectives: AND, OR, NOT, IMPLIES, IFF

;; Inputs

(defparameter *inputs* '((0 0)
			 (0 1)
			 (1 0)
			 (1 1)))

;; User input constants
;; Placeholder ;;;;;;;;;;
(defparameter *connectives* '("AND"
			      "OR"
			      "NOT"
			      "IMPLIES"
			      "IFF"))
;; User interface

(defun main-interface ()
  "Gets user input of logical formula and returns upcased string of input."
  (format t "~&Hello. Please input a logical formula, using letters and connectives
 NOT, AND, OR and IMPLIES e.g. 'P IMPLIES Q' or 'NOT(A) AND B' without the quotes.")
  ;(format t "~&Surround all expressions with brackets, e.g. (A AND B)")
  (format t "~&Fomula: ")
  (let ((user-input (read-line)))
    (string-upcase user-input)))

;; Printing functions

(defun print-truth-table (table)
  (iter (for row in table)
    (format t "~&~S" row)))

(defun make-bracketed (text)
  "Takes text and surrounds it with brackets."
  (format nil "(~a)" text))

;; User input functions
;; TODO ;;;;;;;;;;;;;;;

;; Rules for user interface
;; Individual characters treated as propositions
;; Anything in list *CONNECTIVES* is a connective
;; Expressions must be bracketed

(defun split-expression (input)
  "Splits and expression on spaces and returns the split expression."
  (let ((expression-list (split-sequence:split-sequence #\Space (string-upcase input))))
    expression-list))

(defun not-p (symbol-string)
  (or (string= symbol-string "NOT(" :start1 0 :end1 3 :start2 0 :end2 3)
      (string= symbol-string "(NOT(" :start1 0 :end1 4 :start2 0 :end2 4)))

	  
(defun extract-brackets (input)
  "Returns list of substrings inside brackets, excluding brackets"
  (let ((list-of-brackets '())
	(return-list '()))
    (setf list-of-brackets (cl-ppcre:all-matches-as-strings "\\([\\w\\s]*?\\)" input))
    ;; Trim brackets
    (iter (for string in list-of-brackets)
      (push (string-trim "()" string) return-list))
    (reverse return-list)))

(defun extract-nots (input)
  "Returns list of substrings as 'NOT(<contents of brackets>)'"
  (let ((list-of-nots '())
	(return-list '()))
    (setf list-of-nots (cl-ppcre:all-matches-as-strings "NOT\\([\\w\\s]*?\\)" input))
    list-of-nots))
    ;; Deprecated, may need again so retained as comment
    ;; Trim "NOT(" and ")"
    ;; (iter (for string in list-of-nots)
    ;;   (push (string-left-trim "NOT(" (string-right-trim ")" string)) return-list))
    ;; (reverse return-list)))

(defun proposition-p (x)
  (let ((trimmed-x (string-trim "()" x)))  
    (not (member trimmed-x *connectives* :test #'equal))))


(defun extract-connective (expression)
  "Takes expression with two propositions and returns connective type as string e.g. 'IMPLIES'"
  (second (split-sequence:split-sequence #\Space expression)))

(defun extract-propositions (input)
  "Returns propositions of expression in order they appear in expression."
  (let ((propositions (extract-nots input))
	(symbol-list (split-sequence:split-sequence #\Space input)))
    (iter (for symbol in symbol-list)
      (if (proposition-p symbol) (push symbol propositions)))
    (reverse propositions)))

(defun negate (p)
  (format nil "NOT(~a)" p))

;; Propositional Logic Functions

;; DNF and CNF
(defun pair-cnf (expression)
  "Takes expression as string (only of form p connective p, e.g. 'A AND B') and produces CNF"
  (let ((table (truth-table *inputs* (identify-pair-expression expression)))
	(propositions (extract-propositions expression))
	(false-row-list '())
	(disjunctions '()))
    (iter (for row in table) ; Get false rows
      (if (equal (third row) nil)
	  (push row false-row-list)))
    (setf false-row-list (reverse false-row-list))
    (iter (for row in false-row-list) ; Construct CNF
      (push (construct-disjunction row propositions) disjunctions))
    (setf disjunctions (reverse disjunctions))
    (format t "~&Truth table:")
    (print-truth-table table)
    (format t "~&Conjunctive Normal Form:")
    (format t "~&~a" (conjunct-expression-list disjunctions))))

(defun construct-disjunction (truth-table-row propositions)
  "Takes truth-table-row and returns disjunction for CNF"
  (let ((disjunction "OR")
	(a (first propositions))
	(b (second propositions)))
    (cond ((= (first truth-table-row) 1) (setf a (negate a)))
	  ((= (second truth-table-row) 1) (setf b (negate b))))
    (format nil "~a ~a ~a" a disjunction b)))

(defun construct-conjunction (truth-table-row propositions)
  "Takes truth-table-row and constructs conjunction for DNF"
  (let ((conjunction "AND")
	(a (first propositions))
	(b (second propositions)))
    (cond ((= (first truth-table-row) 0) (setf a (negate a)))
	  ((= (second truth-table-row 0) (setf b (negate b)))))
    (format nil "~a ~a ~a" a conjunction b)))

(defun conjunct-expression-list (expression-list)
  "Takes list of expressions and joins them with ANDs, returns as string"
  (let ((conjunct-string ""))
    (iter (for expression in expression-list)
      (setf conjunct-string (format nil "~a (~a) AND" conjunct-string expression)))
  (string-right-trim " AND" (string-left-trim " " conjunct-string))))


(defun disjunct-expression-list (expression-list)
  "Takes list of expression and joins them with ORs returns as string"
  (let ((disjunct-string ""))
    (iter (for expression in expression-list)
      (setf disjunct-string (format nil "~a (~a) OR" disjunct-string expression)))
  (string-right-trim " OR" (string-left-trim " " disjunct-string))))


    
;; Connectives	
(defun truth-p (x)
  "Returns T if x == 1, which is considered truth, and NIL otherwise."
  (= x 1))

(defun tea-not (p)
  (if (= p 1) nil t))

(defun implies (p q)
  "0 (false) only when P = 1 (true) and Q = 0 (false)"
  (if (and (= p 1) (= q 0)) nil t))

(defun iff (p q)
  "If and only if, biconditional, true when
   both statements are true and when both statements are false,
   false otherwise. Returns T if (p, q == 1) or (p, q == 0)."
  (if (or (and (= p 1) (= q 1))
	  (and (= p 0) (= q 0)))
      t nil))

(defun tea-and (propositions)
  "Returns T if all inputs are 1, NIL otherwise."
  (every #'truth-p propositions))

(defun tea-or (propositions)
  "Returns T if any input is 1, NIL otherwise."
  (some #'truth-p propositions))


;; Truth table calculators


;; (defun generate-inputs (num-p)
;;   (let ((table '())
;; 	(p-counters (make-list num-p :initial-element 0)))
;;     (while (member p-counters 0)
		         
(defun not-truth-table (inputs)
  (let ((table '()))
    (iter (for pair in inputs)
      (setf table (append table (list (list (second pair) (tea-not (second pair)))))))
    table))

(defun implies-truth-table (inputs)
  "Takes a table of inputs in pairs '((0 0) (0 1) ...))
  and returns a truth table for IMPLIES."
  (let ((table '()))
    (iter (for pair in inputs)
      (setf table (append table (list (list (first pair) (second pair) (implies (first pair) (second pair)))))))
    table))

(defun and-truth-table (inputs)
  "Returns AND truth table as table (list of lists) from inputs as table."
  (let ((table '()))
    (iter (for input-group in inputs)
      (setf table (append table (list (list (first input-group) (second input-group) (tea-and input-group))))))
    table))

(defun or-truth-table (inputs)
  "Returns OR truth table as table (list of lists) from inputs as table."
  (let ((table '()))
    (iter (for input-group in inputs)
      (setf table (append table (list (list (first input-group) (second input-group) (tea-or input-group))))))
    table))

(defun iff-truth-table (inputs)
  "Returns iff truth table as table (list of lists) from inputs as table."
  (let ((table '()))
    (iter (for pair in inputs)
      (setf table (append table (list (list (first pair) (second pair) (iff (first pair) (second pair)))))))
    table))
  

(defun truth-table (inputs connective)
  "Returns a truth table for the table inputs from connective as string."
  (cond ((equal connective "NOT") (not-truth-table inputs))
	((equal connective "IMPLIES") (implies-truth-table inputs))
	((equal connective "AND") (and-truth-table inputs))
	((equal connective "OR") (or-truth-table inputs))
	((equal connective "IFF") (iff-truth-table inputs))
	(t nil)))

;; Boolean logic Functions
;; for future expansion

(defun tea-nand (propositions)
  "NAND connective, returns NIL if all inputs are 1, T otherwise."
  (not (tea-and propositions)))

(defun zero-p (x)
  (= x 0))

(defun tea-nor (propositions)
  "NOR connective, returns T if all inputs are 0, NIL otherwise."
  (every #'zero-p propositions))

(defun is-odd-p (x)
  "Returns T if x is odd"
  (not (= (mod x 2) 0)))

(defun tea-xor (propositions)
  "Exclusive OR, T if input of 1's is odd number"
  (let ((oddness-counter 0))
    (iter (for p in propositions)
      (when (truth-p p)
	(incf oddness-counter)))
    (is-odd-p oddness-counter)))

(defun tea-xnor (propositions)
  "T if input of 1's is even"
  (not (tea-xor propositions)))

