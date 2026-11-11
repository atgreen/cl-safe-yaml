;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(defpackage #:cl-safe-yaml
  (:use #:cl)
  (:export
   #:parse-yaml
   #:yaml-parse-error
   #:yaml-parse-error-line
   #:yaml-parse-error-message))

