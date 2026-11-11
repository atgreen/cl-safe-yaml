;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(require "asdf")

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-pathname*))))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,root)
     (:tree ,(merge-pathnames "ocicl/" root))
     :inherit-configuration)))

(asdf:load-system "cl-safe-yaml")

(multiple-value-bind (document duplicates)
    (cl-safe-yaml:parse-yaml
     (format nil "name: example~%ports: [8080, 8443]~%enabled: true~%"))
  (format t "~A has ~D ports; duplicates: ~D~%"
          (gethash "name" document)
          (length (gethash "ports" document))
          (length duplicates)))
