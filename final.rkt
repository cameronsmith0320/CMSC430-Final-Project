#lang racket

(require "closure-convert.rkt"
         "cps.rkt"
         "desugar.rkt"
         "top-level.rkt"
         "utils.rkt")

(provide full-compile)

(define (full-compile e)
  (define tl (top-level e))
  (define ir1 (desugar tl))
  (define ir2 (simplify-ir ir1))
  (define asn (assignment-convert ir2))
  (define aph (alphatize asn))
  (define anf (anf-convert aph))
  (define cps (cps-convert anf))
  (define p (closure-convert cps))
  (proc->llvm p))