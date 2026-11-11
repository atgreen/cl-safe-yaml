;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-safe-yaml)

(define-condition yaml-parse-error (error)
  ((line :initarg :line :reader yaml-parse-error-line)
   (message :initarg :message :reader yaml-parse-error-message))
  (:report (lambda (condition stream)
             (format stream "YAML line ~D: ~A"
                     (yaml-parse-error-line condition)
                     (yaml-parse-error-message condition)))))

(defparameter +yaml-maximum-depth+ 64
  "Deepest accepted nesting, bounding recursion on hostile documents.")

(defstruct (yaml-cursor (:conc-name yaml-))
  (text "" :type simple-string)
  (position 0 :type fixnum)
  (duplicates nil :type list))

(defun yaml-error (cursor control &rest arguments)
  "Reject a malformed document with its one-based line number."
  (error 'yaml-parse-error
         :line (yaml-line cursor)
         :message (apply #'format nil control arguments)))

(defun yaml-line (cursor)
  "Return the one-based line of CURSOR, as Go's YAML decoder counts them."
  (1+ (count #\Newline (yaml-text cursor) :end (min (yaml-position cursor) (length (yaml-text cursor))))))

(defun yaml-column (cursor)
  "Return the zero-based column of CURSOR within its line."
  (let* ((position (min (yaml-position cursor) (length (yaml-text cursor))))
         (newline (position #\Newline (yaml-text cursor) :end position :from-end t)))
    (if newline (- position newline 1) position)))

(defun yaml-peek (cursor &optional (offset 0))
  "Return the character OFFSET past CURSOR, or NIL beyond the end."
  (let ((position (+ (yaml-position cursor) offset)))
    (when (< position (length (yaml-text cursor))) (schar (yaml-text cursor) position))))

(defun yaml-end-of-token-p (character)
  "Recognize the characters that end an indicator such as '- ' or ': '."
  (member character '(nil #\Space #\Tab #\Newline #\Return)))

(defun yaml-skip-line-space (cursor)
  "Skip spaces and a trailing comment, stopping at the line's end."
  (loop for character = (yaml-peek cursor)
        while (member character '(#\Space #\Tab #\Return)) do (incf (yaml-position cursor)))
  (when (and (eql #\# (yaml-peek cursor))
             (or (zerop (yaml-position cursor))
                 (yaml-end-of-token-p (schar (yaml-text cursor) (1- (yaml-position cursor))))))
    (loop until (member (yaml-peek cursor) '(nil #\Newline)) do (incf (yaml-position cursor)))))

(defun yaml-document-marker-p (cursor marker)
  "Recognize '---' or '...' at the start of a line."
  (and (zerop (yaml-column cursor))
       (let ((end (+ (yaml-position cursor) 3)))
         (and (<= end (length (yaml-text cursor)))
              (string= marker (yaml-text cursor) :start2 (yaml-position cursor) :end2 end)
              (yaml-end-of-token-p (yaml-peek cursor 3))))))

(defun yaml-skip-to-token (cursor)
  "Move to the next meaningful character, across blank and comment lines."
  (loop
    (yaml-skip-line-space cursor)
    (if (eql #\Newline (yaml-peek cursor)) (incf (yaml-position cursor)) (return))))

(defun yaml-at-end-p (cursor)
  "Report whether the current document has no further tokens."
  (or (null (yaml-peek cursor)) (yaml-document-marker-p cursor "---") (yaml-document-marker-p cursor "...")))

(defun yaml-reject-unsupported (cursor)
  "Refuse node properties instead of treating them as plain text."
  (let ((character (yaml-peek cursor)))
    (when (member character '(#\& #\* #\!))
      (yaml-error cursor "anchors, aliases and tags are not supported"))))

(defun yaml-read-double-quoted (cursor)
  "Read a double-quoted scalar, folding line breaks and decoding its escapes."
  (incf (yaml-position cursor))
  (with-output-to-string (out)
    (loop
      (let ((character (yaml-peek cursor)))
        (incf (yaml-position cursor))
        (case character
          ((nil) (yaml-error cursor "found unexpected end of stream in quoted scalar"))
          (#\" (return))
          (#\Newline
           (let ((breaks 0))
             (loop for next = (yaml-peek cursor)
                   while (member next '(#\Space #\Tab #\Newline))
                   do (when (eql next #\Newline) (incf breaks)) (incf (yaml-position cursor)))
             (if (zerop breaks) (write-char #\Space out) (dotimes (index breaks) (write-char #\Newline out)))))
          (#\\
           (let ((escape (yaml-peek cursor)))
             (incf (yaml-position cursor))
             (flet ((code (digits)
                      (let ((end (+ (yaml-position cursor) digits)))
                        (unless (<= end (length (yaml-text cursor))) (yaml-error cursor "invalid escape"))
                        (let ((value (or (parse-integer (yaml-text cursor) :start (yaml-position cursor) :end end
                                                                           :radix 16 :junk-allowed t)
                                         (yaml-error cursor "invalid escape"))))
                          (setf (yaml-position cursor) end)
                          (write-char (code-char value) out)))))
               (case escape
                 (#\0 (write-char (code-char 0) out)) (#\a (write-char (code-char 7) out))
                 (#\b (write-char #\Backspace out)) (#\t (write-char #\Tab out)) (#\Tab (write-char #\Tab out))
                 (#\n (write-char #\Newline out)) (#\v (write-char (code-char 11) out))
                 (#\f (write-char #\Page out)) (#\r (write-char #\Return out)) (#\e (write-char (code-char 27) out))
                 (#\Space (write-char #\Space out)) (#\" (write-char #\" out)) (#\/ (write-char #\/ out))
                 (#\\ (write-char #\\ out)) (#\N (write-char (code-char #x85) out)) (#\_ (write-char (code-char #xa0) out))
                 (#\L (write-char (code-char #x2028) out)) (#\P (write-char (code-char #x2029) out))
                 (#\x (code 2)) (#\u (code 4)) (#\U (code 8))
                 (#\Newline (loop while (member (yaml-peek cursor) '(#\Space #\Tab)) do (incf (yaml-position cursor))))
                 (t (yaml-error cursor "found unknown escape character"))))))
          (t (write-char character out)))))))

(defun yaml-read-single-quoted (cursor)
  "Read a single-quoted scalar, where only a doubled quote is an escape."
  (incf (yaml-position cursor))
  (with-output-to-string (out)
    (loop
      (let ((character (yaml-peek cursor)))
        (incf (yaml-position cursor))
        (case character
          ((nil) (yaml-error cursor "found unexpected end of stream in quoted scalar"))
          (#\' (if (eql #\' (yaml-peek cursor))
                   (progn (incf (yaml-position cursor)) (write-char #\' out))
                   (return)))
          (#\Newline
           (let ((breaks 0))
             (loop for next = (yaml-peek cursor)
                   while (member next '(#\Space #\Tab #\Newline))
                   do (when (eql next #\Newline) (incf breaks)) (incf (yaml-position cursor)))
             (if (zerop breaks) (write-char #\Space out) (dotimes (index breaks) (write-char #\Newline out)))))
          (t (write-char character out)))))))

(defun yaml-plain-end (cursor flow)
  "Return where a plain scalar on this line stops, before ': ', ' #' or a flow indicator."
  (let ((text (yaml-text cursor)))
    (loop for position from (yaml-position cursor) below (length text)
          for character = (schar text position)
          for next = (when (< (1+ position) (length text)) (schar text (1+ position)))
          do (cond ((member character '(#\Newline)) (return position))
                   ((and (char= character #\:) (or (yaml-end-of-token-p next) (and flow (find next ",[]{}")))) (return position))
                   ((and (char= character #\#) (> position (yaml-position cursor))
                         (yaml-end-of-token-p (schar text (1- position)))) (return position))
                   ((and flow (find character ",[]{}")) (return position)))
          finally (return (length text)))))

(defun yaml-resolve-plain (token)
  "Give a plain scalar the JSON type that YAML 1.1 implicit typing assigns it."
  (let ((integer-text (string-left-trim "+" token)))
    (cond
      ((member token '("" "~" "null" "Null" "NULL") :test #'string=) 'null)
      ((member token '("true" "True" "TRUE" "yes" "Yes" "YES" "on" "On" "ON") :test #'string=) t)
      ((member token '("false" "False" "FALSE" "no" "No" "NO" "off" "Off" "OFF") :test #'string=) nil)
      ((cl-ppcre:scan "^[-+]?0[0-7]+$" token) (parse-integer token :radix 8))
      ((cl-ppcre:scan "^[-+]?0x[0-9a-fA-F]+$" token)
       (let ((negative (char= #\- (char token 0))) (digits (subseq token (+ 2 (position #\0 token)))))
         (* (if negative -1 1) (parse-integer digits :radix 16))))
      ((cl-ppcre:scan "^[-+]?[0-9]+$" token) (parse-integer integer-text))
      ((cl-ppcre:scan "^[-+]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][-+]?[0-9]+)?$" token)
       (com.inuoe.jzon:parse (cl-ppcre:regex-replace "\\.([eE]|$)"
                                                     (cl-ppcre:regex-replace "^([-]?)\\." integer-text "\\{1}0.")
                                                     "\\1")))
      (t token))))

(defun yaml-read-plain (cursor indent)
  "Read a plain scalar, folding continuation lines indented deeper than INDENT."
  (let* ((end (yaml-plain-end cursor nil))
         (parts (list (string-right-trim '(#\Space #\Tab #\Return) (subseq (yaml-text cursor) (yaml-position cursor) end)))))
    (setf (yaml-position cursor) end)
    (loop
      (let ((saved (yaml-position cursor)))
        (yaml-skip-line-space cursor)
        (unless (eql #\Newline (yaml-peek cursor)) (setf (yaml-position cursor) saved) (return))
        (yaml-skip-to-token cursor)
        (when (or (yaml-at-end-p cursor) (<= (yaml-column cursor) indent) (eql #\# (yaml-peek cursor)))
          (setf (yaml-position cursor) saved) (return))
        (let ((next-end (yaml-plain-end cursor nil)))
          (when (and (< next-end (length (yaml-text cursor))) (char= #\: (schar (yaml-text cursor) next-end)))
            (yaml-error cursor "mapping values are not allowed in this context"))
          (push (string-right-trim '(#\Space #\Tab #\Return) (subseq (yaml-text cursor) (yaml-position cursor) next-end)) parts)
          (setf (yaml-position cursor) next-end))))
    (let ((token (format nil "~{~A~^ ~}" (reverse parts))))
      (if (rest parts) token (yaml-resolve-plain token)))))

(defun yaml-read-block-scalar (cursor indent)
  "Read a literal or folded block scalar nested under INDENT, honoring chomping."
  (let* ((folded (char= #\> (yaml-peek cursor))) (chomp nil) (explicit nil))
    (incf (yaml-position cursor))
    (loop for character = (yaml-peek cursor)
          while (and character (or (find character "+-") (digit-char-p character)))
          do (if (digit-char-p character) (setf explicit (digit-char-p character)) (setf chomp character))
             (incf (yaml-position cursor)))
    (yaml-skip-line-space cursor)
    (unless (member (yaml-peek cursor) '(nil #\Newline)) (yaml-error cursor "did not find expected comment or line break"))
    (when (yaml-peek cursor) (incf (yaml-position cursor)))
    (let ((lines nil) (block-indent (when explicit (+ (max indent 0) explicit))))
      (loop
        (when (null (yaml-peek cursor)) (return))
        (let* ((text (yaml-text cursor)) (start (yaml-position cursor))
               (end (or (position #\Newline text :start start) (length text)))
               (line (string-right-trim '(#\Return) (subseq text start end)))
               (width (or (position #\Space line :test-not #'char=) (length line))))
          (cond
            ((= width (length line)) (push "" lines))
            (t
             (unless block-indent
               (when (<= width indent) (return))
               (setf block-indent width))
             (when (< width block-indent) (return))
             (when (and (zerop width) (member (subseq line 0 (min 3 (length line))) '("---" "...") :test #'equal)) (return))
             (push (subseq line block-indent) lines)))
          (setf (yaml-position cursor) (min (1+ end) (length text)))))
      (let* ((trailing (loop for line in lines while (equal line "") count t))
             (content (reverse (nthcdr trailing lines)))
             (body (with-output-to-string (out)
                     (loop for (line next) on content
                           do (write-string line out)
                              (when next
                                (cond ((not folded) (write-char #\Newline out))
                                      ((equal next "") (write-char #\Newline out))
                                      ((equal line "") nil)
                                      ((or (find (char next 0) '(#\Space #\Tab)) (find (char line 0) '(#\Space #\Tab))) (write-char #\Newline out))
                                      (t (write-char #\Space out))))))))
        (cond ((null content) (if (eql chomp #\+) (make-string trailing :initial-element #\Newline) ""))
              ((eql chomp #\-) body)
              ((eql chomp #\+) (concatenate 'string body (make-string (1+ trailing) :initial-element #\Newline)))
              (t (concatenate 'string body (string #\Newline))))))))

(defun yaml-skip-flow-space (cursor)
  "Skip whitespace and comments inside a flow collection, where lines do not matter."
  (yaml-skip-to-token cursor)
  (unless (yaml-peek cursor) (yaml-error cursor "did not find expected node content")))

(defun yaml-read-flow-scalar (cursor)
  "Read a scalar in a flow collection, where commas and brackets end plain text."
  (case (yaml-peek cursor)
    (#\" (yaml-read-double-quoted cursor))
    (#\' (yaml-read-single-quoted cursor))
    (t (yaml-reject-unsupported cursor)
     (let ((end (yaml-plain-end cursor t)))
       (prog1 (yaml-resolve-plain (string-right-trim '(#\Space #\Tab #\Return) (subseq (yaml-text cursor) (yaml-position cursor) end)))
         (setf (yaml-position cursor) end))))))

(defun yaml-note-key (cursor object key line)
  "Record a repeated mapping key and let the last value win, as a lenient decoder does."
  (when (nth-value 1 (gethash key object))
    (setf (yaml-duplicates cursor)
          (append (yaml-duplicates cursor) (list (format nil "line ~D: key ~S already set in map" line key))))))

(defun yaml-key-string (cursor key)
  "Require a JSON-representable mapping key."
  (cond ((stringp key) key)
        ((integerp key) (format nil "~D" key))
        ((eq key t) "true") ((null key) "false") ((eq key 'null) "null")
        ((numberp key) (com.inuoe.jzon:stringify key))
        (t (yaml-error cursor "invalid map key"))))

(defun yaml-read-flow (cursor depth)
  "Read a flow mapping, sequence or scalar, which may span lines freely."
  (when (> depth +yaml-maximum-depth+) (yaml-error cursor "exceeded max depth of ~D" +yaml-maximum-depth+))
  (yaml-skip-flow-space cursor)
  (case (yaml-peek cursor)
    (#\{
     (incf (yaml-position cursor))
     (let ((object (make-hash-table :test #'equal)))
       (loop
         (yaml-skip-flow-space cursor)
         (when (eql #\} (yaml-peek cursor)) (incf (yaml-position cursor)) (return object))
         (let* ((line (yaml-line cursor))
                (key (yaml-key-string cursor (yaml-read-flow-scalar cursor))))
           (yaml-skip-flow-space cursor)
           (yaml-note-key cursor object key line)
           (if (eql #\: (yaml-peek cursor))
               (progn (incf (yaml-position cursor))
                      (yaml-skip-flow-space cursor)
                      (setf (gethash key object)
                            (if (find (yaml-peek cursor) ",}") 'null (yaml-read-flow cursor (1+ depth)))))
               (setf (gethash key object) 'null)))
         (yaml-skip-flow-space cursor)
         (case (yaml-peek cursor)
           (#\, (incf (yaml-position cursor)))
           (#\})
           (t (yaml-error cursor "did not find expected ',' or '}'"))))))
    (#\[
     (incf (yaml-position cursor))
     (let ((items nil))
       (loop
         (yaml-skip-flow-space cursor)
         (when (eql #\] (yaml-peek cursor)) (incf (yaml-position cursor)) (return (coerce (nreverse items) 'vector)))
         (push (yaml-read-flow cursor (1+ depth)) items)
         (yaml-skip-flow-space cursor)
         (case (yaml-peek cursor)
           (#\, (incf (yaml-position cursor)))
           (#\])
           (t (yaml-error cursor "did not find expected ',' or ']'"))))))
    (t (yaml-read-flow-scalar cursor))))

(defun yaml-mapping-key-ahead-p (cursor)
  "Report whether the token at CURSOR is a block mapping key rather than a scalar."
  (let ((saved (yaml-position cursor)) (character (yaml-peek cursor)))
    (unwind-protect
         (case character
           ((#\" #\')
            (if (char= character #\") (yaml-read-double-quoted cursor) (yaml-read-single-quoted cursor))
            (loop while (member (yaml-peek cursor) '(#\Space #\Tab)) do (incf (yaml-position cursor)))
            (and (eql #\: (yaml-peek cursor)) (yaml-end-of-token-p (yaml-peek cursor 1))))
           ((#\{ #\[) nil)
           (t (setf (yaml-position cursor) (yaml-plain-end cursor nil))
              (and (eql #\: (yaml-peek cursor)) (yaml-end-of-token-p (yaml-peek cursor 1)))))
      (setf (yaml-position cursor) saved))))

(defun yaml-read-mapping (cursor indent depth)
  "Read block mapping entries whose keys all start at column INDENT."
  (let ((object (make-hash-table :test #'equal)))
    (loop
      (let* ((line (yaml-line cursor))
             (key (yaml-key-string cursor
                                   (case (yaml-peek cursor)
                                     (#\" (yaml-read-double-quoted cursor))
                                     (#\' (yaml-read-single-quoted cursor))
                                     (t (let ((end (yaml-plain-end cursor nil)))
                                          (prog1 (yaml-resolve-plain (string-right-trim '(#\Space #\Tab) (subseq (yaml-text cursor) (yaml-position cursor) end)))
                                            (setf (yaml-position cursor) end))))))))
        (loop while (member (yaml-peek cursor) '(#\Space #\Tab)) do (incf (yaml-position cursor)))
        (unless (and (eql #\: (yaml-peek cursor)) (yaml-end-of-token-p (yaml-peek cursor 1)))
          (yaml-error cursor "could not find expected ':'"))
        (incf (yaml-position cursor))
        (yaml-note-key cursor object key line)
        (when (equal key "<<") (yaml-error cursor "merge keys are not supported"))
        (setf (gethash key object) (yaml-read-mapping-value cursor indent depth)))
      (yaml-skip-to-token cursor)
      (when (yaml-at-end-p cursor) (return object))
      (let ((column (yaml-column cursor)))
        (when (< column indent) (return object))
        (when (> column indent) (yaml-error cursor "did not find expected key"))))))

(defun yaml-read-mapping-value (cursor indent depth)
  "Read a value after 'key:', either inline or as a deeper block."
  (yaml-skip-line-space cursor)
  (if (member (yaml-peek cursor) '(nil #\Newline))
      (yaml-read-node cursor indent (1+ depth) t)
      (progn
        (yaml-reject-unsupported cursor)
        (when (member (yaml-peek cursor) '(#\| #\>))
          (return-from yaml-read-mapping-value (yaml-read-block-scalar cursor indent)))
        (let ((value
                (case (yaml-peek cursor)
                  ((#\{ #\[) (yaml-read-flow cursor (1+ depth)))
                  (#\" (yaml-read-double-quoted cursor))
                  (#\' (yaml-read-single-quoted cursor))
                  (t (if (and (eql #\- (yaml-peek cursor)) (yaml-end-of-token-p (yaml-peek cursor 1)))
                         (yaml-error cursor "block sequence entries are not allowed in this context")
                         (yaml-read-plain cursor indent))))))
          (yaml-finish-line cursor)
          value))))

(defun yaml-finish-line (cursor)
  "Require nothing but a comment after an inline value."
  (yaml-skip-line-space cursor)
  (unless (member (yaml-peek cursor) '(nil #\Newline))
    (yaml-error cursor (if (eql #\: (yaml-peek cursor))
                           "mapping values are not allowed in this context"
                           "did not find expected key"))))

(defun yaml-read-sequence (cursor indent depth)
  "Read block sequence entries whose dashes all start at column INDENT."
  (let ((items nil))
    (loop
      (incf (yaml-position cursor))
      (push (yaml-read-node cursor indent (1+ depth) nil) items)
      (yaml-skip-to-token cursor)
      (unless (and (not (yaml-at-end-p cursor)) (= indent (yaml-column cursor))
                   (eql #\- (yaml-peek cursor)) (yaml-end-of-token-p (yaml-peek cursor 1)))
        (when (and (not (yaml-at-end-p cursor)) (> (yaml-column cursor) indent))
          (yaml-error cursor "did not find expected '-' indicator"))
        (return (coerce (nreverse items) 'vector))))))

(defun yaml-read-node (cursor indent depth sequence-at-indent)
  "Read the next node if it lies deeper than INDENT, or null when none does.
SEQUENCE-AT-INDENT admits a mapping value's sequence written at the key's own column."
  (when (> depth +yaml-maximum-depth+) (yaml-error cursor "exceeded max depth of ~D" +yaml-maximum-depth+))
  (yaml-skip-to-token cursor)
  (when (yaml-at-end-p cursor) (return-from yaml-read-node 'null))
  (let* ((column (yaml-column cursor)) (character (yaml-peek cursor))
         (dash (and (eql character #\-) (yaml-end-of-token-p (yaml-peek cursor 1)))))
    (when (< column (if (and sequence-at-indent dash) indent (1+ indent)))
      (return-from yaml-read-node 'null))
    (yaml-reject-unsupported cursor)
    (cond
      (dash (yaml-read-sequence cursor column depth))
      ((member character '(#\| #\>)) (yaml-read-block-scalar cursor indent))
      ((member character '(#\{ #\[)) (prog1 (yaml-read-flow cursor depth) (yaml-finish-line cursor)))
      ((yaml-mapping-key-ahead-p cursor) (yaml-read-mapping cursor column depth))
      ((char= character #\") (prog1 (yaml-read-double-quoted cursor) (yaml-finish-line cursor)))
      ((char= character #\') (prog1 (yaml-read-single-quoted cursor) (yaml-finish-line cursor)))
      (t (prog1 (yaml-read-plain cursor indent) (yaml-finish-line cursor))))))

(defun parse-yaml (text)
  "Decode TEXT's first YAML document into ordinary Common Lisp data.
Returns the value and a list of repeated-key descriptions in document order."
  (let ((cursor (make-yaml-cursor :text (coerce text 'simple-string))))
    (yaml-skip-to-token cursor)
    (when (yaml-document-marker-p cursor "---") (incf (yaml-position cursor) 3))
    (let ((value (yaml-read-node cursor -1 0 nil)))
      (yaml-skip-to-token cursor)
      (unless (yaml-at-end-p cursor) (yaml-error cursor "did not find expected <document start>"))
      (values value (yaml-duplicates cursor)))))

