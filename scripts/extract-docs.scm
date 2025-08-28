#!/usr/bin/env -S guile-3.0 -e main -s
!#
;; Extract Guix module docs from docstrings into a Markdown file.
;; Produces: build/guix-modules.md
;; Requires GUILE_LOAD_PATH to include the Guix checkout (the repo top-level).

(use-modules ;(ice-9 modules)
             (ice-9 documentation)
             (ice-9 pretty-print)
             (ice-9 format)
             (srfi srfi-1))

(define major-modules
  '((guix)
    (guix utils)
    (guix records)
    (guix licenses)
    (guix base32)
    (guix download)
    (guix git)
    (guix gexp)
    (guix packages)
    (guix profiles)
    (guix store)
    (guix build-system)
    (guix build-system gnu)
    (guix build-system cmake)
    (guix build-system go)
    (guix build-system python)
    (guix build-system emacs)))

(define (safe thunk)
  (catch #t (lambda () (thunk))
             (lambda args #f)))

(define (var->doc var)
  (safe
   (lambda ()
     (let ((val (and (variable-bound? var) (variable-ref var))))
       (or (and val (procedure? val) (procedure-documentation val))
           (variable-documentation var)
           #f)))))

(define (module->entries m)
  (let ((entries '())
        (iface (module-public-interface m)))
    (module-map
     (lambda (sym var)
       (let ((doc (var->doc var)))
         (when (and doc (string? doc) (not (string-null? doc)))
           (set! entries (cons (list sym doc) entries)))))
     iface)
    (sort entries (lambda (a b)
                    (string-ci<? (symbol->string (car a))
                                 (symbol->string (car b)))))))

(define (module-name->string name)
  (string-join (map symbol->string name) " "))

(define (mkdir-p path)
  (false-if-exception (mkdir path #o755)))

(define (doc-module port name)
  (let ((m (safe (lambda () (resolve-interface name)))))
    (if (not m)
        (format port "\n# Module (~a)\n\nCould not load this module (skipped).\n"
                (module-name->string name))
        (let* ((entries (module->entries m)))
          (format port "\n# Module (~a)\n\n" (module-name->string name))
          (if (null? entries)
              (format port "_No documented bindings found._\n\n")
              (for-each
               (lambda (pair)
                 (let ((sym (car pair)) (doc (cadr pair)))
                   (format port "## ~a\n\n" (symbol->string sym))
                   ;; Docstrings are often Texinfo; we keep them as-is.
                   (format port "~a\n\n" doc)))
               entries))))))

(define (main args)
  (let ((out-dir "build")
        (out-file "build/guix-modules.md"))
    (mkdir-p out-dir)
    (call-with-output-file out-file
      (lambda (port)
        (format port "# Guix API (docstrings dump)\n\n")
        (for-each (lambda (name) (doc-module port name))
                  major-modules)))
    (format #t "Wrote ~a\n" out-file)))
