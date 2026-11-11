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

