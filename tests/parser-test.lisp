;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-safe-yaml/tests)
(def-suite yaml-decoding :in cl-safe-yaml)
(in-suite yaml-decoding)

(defun yaml-json (text)
  "Decode TEXT and render it as canonical JSON for comparison."
  (com.inuoe.jzon:stringify (values (cl-safe-yaml:parse-yaml text))))

(test yaml-decodes-nested-documents
  (let ((object (cl-safe-yaml:parse-yaml (format nil "~%apiVersion: mygroup.example.com/v1~%kind: Foo~%metadata:~%  name: mytest # comment~%spec:~%  foo: foo1~%  cronSpec: \"* * * * */5\"~%  ports:~%  - name: x~%    containerPort: 80~%    protocol: TCP~%  - name: y~%    containerPort: 81~%"))))
    (is (equal "Foo" (gethash "kind" object)))
    (is (equal "mytest" (gethash "name" (gethash "metadata" object))))
    (let ((spec (gethash "spec" object)))
      (is (equal "* * * * */5" (gethash "cronSpec" spec)))
      (is (= 2 (length (gethash "ports" spec))))
      (is (= 80 (gethash "containerPort" (aref (gethash "ports" spec) 0))))
      (is (equal "TCP" (gethash "protocol" (aref (gethash "ports" spec) 0))))
      (is (equal "y" (gethash "name" (aref (gethash "ports" spec) 1)))))))

(test yaml-resolves-scalars-as-yaml-does
  (is (equal "[1,-2,1.5,true,false,null,null,\"text\",\"1\",8,\"a b\"]"
             (yaml-json "[1, -2, 1.5, true, false, null, ~, text, '1', 010, \"a\\x20b\"]")))
  (let ((object (cl-safe-yaml:parse-yaml (format nil "a: yes~%b: off~%c:~%d: ''~%e: 1e3~%f: 2021-01-01~%g: foo bar~%"))))
    (is (eq t (gethash "a" object)))
    (is (eq nil (gethash "b" object)))
    (is (eq 'null (gethash "c" object)))
    (is (equal "" (gethash "d" object)))
    (is (= 1000 (gethash "e" object)))
    (is (equal "2021-01-01" (gethash "f" object)))
    (is (equal "foo bar" (gethash "g" object)))))

(test yaml-reads-flow-and-block-collections
  (is (equal "{\"a\":{\"b\":[1,{\"c\":2}]},\"d\":[]}" (yaml-json "{\"a\": {\"b\": [1, {\"c\": 2}]}, \"d\": []}")))
  (is (equal "{\"a\":[[1,2],[3]],\"b\":{\"c\":{}}}"
             (yaml-json (format nil "a:~%- - 1~%  - 2~%- - 3~%b: {c: {}}~%"))))
  (is (equal "{\"a\":[{\"b\":1,\"c\":[2]}]}"
             (yaml-json (format nil "a:~%  - b: 1~%    c:~%      - 2~%")))))

(test yaml-reads-multiline-scalars
  (let ((object (cl-safe-yaml:parse-yaml (format nil "lit: |~%  one~%   two~%~%  three~%fold: >-~%  one~%  two~%~%  three~%keep: |+~%  x~%~%plain: one~%  two~%quoted: \"a~%  b\"~%"))))
    (is (equal (format nil "one~% two~%~%three~%") (gethash "lit" object)))
    (is (equal (format nil "one two~%three") (gethash "fold" object)))
    (is (equal (format nil "x~%~%") (gethash "keep" object)))
    (is (equal "one two" (gethash "plain" object)))
    (is (equal "a b" (gethash "quoted" object)))))

(test yaml-reports-duplicate-keys-with-their-lines
  (multiple-value-bind (object duplicates)
      (cl-safe-yaml:parse-yaml (format nil "~%apiVersion: a/v1~%kind: Foo~%metadata:~%  name: n~%spec:~%  unknown: uk1~%  foo: foo1~%  foo: foo2~%"))
    (is (equal "foo2" (gethash "foo" (gethash "spec" object))))
    (is (equal '("line 9: key \"foo\" already set in map") duplicates))))

(test yaml-rejects-what-it-cannot-represent
  (dolist (text (list "a: &x 1" "a: *x" "a: !!str 1" (format nil "a: 1~%  b: 2") "a: b: c" "[1, 2" (format nil "a:~%  - 1~% b: 2")))
    (signals cl-safe-yaml:yaml-parse-error (cl-safe-yaml:parse-yaml text))))

(test yaml-bounds-nesting
  (signals cl-safe-yaml:yaml-parse-error
    (cl-safe-yaml:parse-yaml (make-string 100 :initial-element #\[))))

(test yaml-reads-only-the-first-document
  (is (equal "{\"a\":1}" (yaml-json (format nil "---~%a: 1~%---~%b: 2~%")))))


(test yaml-line-numbers-survive-backing-up-and-skipping-ahead
  ;; The line is counted on from the last answer, so check it in every order.
  (let* ((text (format nil "a: 1~%b: 2~%c:~%  d: 3~%e: 4~%"))
         (cursor (cl-safe-yaml::make-yaml-cursor :text (coerce text 'simple-string))))
    (flet ((line-at (position)
             (setf (cl-safe-yaml::yaml-position cursor) position)
             (cl-safe-yaml::yaml-line cursor))
           (expected (position) (1+ (count #\Newline text :end position))))
      (dolist (position (list 0 7 (length text) 3 12 12 0 20 5 (+ 10 (length text))))
        (is (= (expected (min position (length text))) (line-at position))
            "line at offset ~D" position)))))

(test yaml-duplicate-keys-still-name-their-line
  (multiple-value-bind (object duplicates)
      (cl-safe-yaml:parse-yaml (format nil "{\"a\": 1,~% \"b\": 2,~% \"a\": 3}"))
    (is (= 2 (hash-table-count object)))
    (is (= 1 (length duplicates)))
    (is (search "3" (princ-to-string (first duplicates))) "the repeat is on line 3")))

(test yaml-reads-a-large-single-line-json-document-in-linear-time
  ;; What kubectl sends for a server-side apply: JSON, on one line, under a YAML
  ;; content type. 40,000 keys took most of a minute when each one recounted
  ;; the text from the top.
  (let* ((text (with-output-to-string (out)
                 (write-char #\{ out)
                 (dotimes (index 40000)
                   (format out "~:[,~;~]\"key~D\":{\"type\":\"string\",\"description\":\"field number ~D\"}"
                           (zerop index) index index))
                 (write-char #\} out)))
         (start (get-internal-real-time))
         (object (cl-safe-yaml:parse-yaml text)))
    (is (= 40000 (hash-table-count object)))
    (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 5)
        "~D characters parsed in under five seconds" (length text))))
