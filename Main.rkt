#lang racket
(require "Utility.rkt")
(require "Runner.rkt")
(require "Parser.rkt")
;(require "Variable_Env.rkt")
(define env '((a 1) (b 2) c 5))

;(define sample-code '(call (function () (ask (bool != a b) (math - a b) (math + a b))) (a)))
;(display (neo-parser sample-code))
;(define parsed-neo-code (neo-parser sample-code))
;(run-neo-parsed-code parsed-neo-code env)

;(element At '(!= (var_exp a) (var_exp b)) 1)

(neo-let-code-parser '(local-vars ((a 1) (b 2) (c 3)) (math + a b)))
(call (function (a) (local-vars ((x 5) (y 6) (z 9)) ((call (function (b)(math + a (math * b x)))) (2)))) (3)