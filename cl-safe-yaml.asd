;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem "cl-safe-yaml"
  :description "A restricted data-only YAML parser"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("cl-ppcre" "com.inuoe.jzon")
  :serial t
  :components ((:module "src" :components ((:file "package") (:file "parser"))))
  :in-order-to ((test-op (test-op "cl-safe-yaml/tests"))))

(asdf:defsystem "cl-safe-yaml/tests"
  :depends-on ("cl-safe-yaml" "fiveam" "com.inuoe.jzon")
  :serial t
  :components ((:module "tests" :components ((:file "package") (:file "parser-test"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :fiveam :run!
                                       (find-symbol "CL-SAFE-YAML" "CL-SAFE-YAML/TESTS"))
               (error "cl-safe-yaml tests failed"))))

