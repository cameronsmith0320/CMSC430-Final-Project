# CMSC430-Final-Project

Overview:
    This project contains a compiler that takes the following grammar as input:
    
    e ::= (define x e)
        | (define (x x ... defaultparam ...) e ...+)
        | (define (x x ... . x) e ...+)
        | (letrec* ([x e] ...) e ...+)
        | (letrec ([x e] ...) e ...+)
        | (let* ([x e] ...) e ...+)
        | (let ([x e] ...) e ...+)
        | (let x ([x e] ...) e ...+)
        | (lambda (x ... defaultparam ...) e ...+)
        | (lambda x e ...+)
        | (lambda (x ...+ . x) e ...+)
        | (dynamic-wind e e e)
        | (guard (x cond-clause ...) e ...+)
        | (raise e)
        | (delay e)
        | (force e)
        | (and e ...)
        | (or e ...)
        | (match e match-clause ...)
        | (cond cond-clause ...)
        | (case e case-clause ...)
        | (if e e e)
        | (when e e ...+)
        | (unless e e ...+)
        | (set! x e)
        | (begin e ...+)
        | (call/cc e)
        | (apply e e)
        | (e e ...)
        | x
        | op
        | (quasiquote qq)
        | (quote dat)
        | nat | string | #t | #f
    cond-clause ::= (e) | (e e e ...) | (else e e ...)
    case-clause ::= ((dat ...) e e ...) | (else e e ...)
    match-clause ::= (pat e e ...) | (else e e ...)
    ;; in all cases, else clauses must come last
    dat is a datum satisfying datum? from utils.rkt
    x is a variable (satisfies symbol?)
    defaultparam ::= (x e)
    op is a symbol satisfying prim? from utils.rkt (if not otherwise in scope)
    op ::= promise? | null? | cons | car | + | ...  (see utils.rkt)
    qq ::= e | dat | (unquote qq) | (unquote e) | (quasiquote qq)
         | (qq ...+) | (qq ...+ . qq)
    ;; (quasiquote has the same semantics as in Racket)
    pat ::= nat | string | #t | #f | (quote dat) | x | (? e pat) | (cons pat pat) | (quasiquote qqpat)
    qqpat ::= e | dat | (unquote qqpat) | (unquote pat) | (quasiquote qq)
            | (qq ...+) | (qq ...+ . qq)
    ;; (same semantics as Racket match for this subset of patterns)
    
    and produces as output a compiled binary in the LLVM language (http://llvm.org/docs/).
    
    This is accomplished through multiple compilation passes. The first, through top-level.rkt, removes (define ...), (quasiquote ...),
    and (match ...) forms from the language, explicitly quotes datums, and explicitly adds (begin ...) forms where they were previously
    implicit. The second pass runs through desugar.rkt, and removes most other forms from the language, leaving the following:
    
    e ::= (let ([x e] ...) e)
        | (lambda (x ...) e)
        | (lambda x e)
        | (apply e e)
        | (e e ...)
        | (prim op e ...)
        | (apply-prim op e)
        | (if e e e)
        | (set! x e)
        | (call/cc e)
        | x
        | (quote dat)
    dat is a datum satisfying datum? from utils.rkt
    x is a variable (satisfies symbol?)
    op is a symbol satisfying prim? from utils.rkt (if not otherwise in scope)
    op ::= promise? | null? | cons | car | + | ...  (see utils.rkt)
    
    This output is then run through a "simplify-ir" pass that adds some additional primitive operations and simplifies others.
    
    The next set of compilation passes is through cps.rkt. The first of these is assignment conversion, in which the (set! ...) form
    is removed. Next is alphatization, in which capture-avoiding substitution is applied to eliminate shadowing of variable names.
    Third is ANF conversion, in which the program is converted into administrative normal form, partitioning it into atomic expressions
    and complex expressions and removing multiple subexpressions from most forms. Fourth is CPS conversion, in which the program is
    converted into continuation-passing style, removing the need for a stack.
    
    The final set of passes runs through closure-convert.rkt, first converting the program into a list of procedures in preparation for the
    ultimate pass, which converts this list into LLVM. Upon evaluation of the compiled code, this LLVM representation is compiled using
    Clang (http://clang.llvm.org/index.html) with a header file (header.cpp) into a binary.
    
    Each step in this process is performed automatically by the full-compile function, which takes an expression in the input grammar and
    outputs LLVM code. This function is located in the final.rkt file.

Primitive Operations:
    The compiler supports the following primitive operations. Each can be applied as described directly (e.g. (+ 2 3)) or by using the (apply ...)
    form (e.g. (apply + '(2 3 4))).
    
    Arithmetic:
        + : (+ int ...) -> int
            Adds each argument in sequence. Returns 0 if applied on no arguments.
            Can raise an "argument not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
        
        - : (- int ...+) -> int
            If given 2 or more arguments, subtracts from the first argument the sum of all remaining arguments.
            If given 1 argument, returns the result of (- 0 arg), where arg is the supplied argument.
            Can raise an "argument not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
        
        * : (* int ...) -> int
            Multiplies each argument in sequence. Returns 1 if applied on no arguments.
            Can raise an "arguemnt not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
        
        / : (/ int int ...+) -> int
            Divides the first argument by each subsequent argument.
            Behavior is undefined if supplied exactly one argument.
            Can raise an "argument not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
            Can raise a "divide by 0" error.
        
    Comparison:
        = : (= int int ...+) -> #t | #f
            Returns #t if all arguments are numerically equal.
            Returns #f otherwise.
            Can raise an "argument not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
        
        > : (> int int ...+) -> #t | #f
            Returns #t if arguments are in strictly decreasing order.
            Returns #f otherwise.
            Can raise an "argument not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
        
        >= : (>= int int ...+) -> #t | #f
             Returns #t if arguments are not in increasing order.
             Returns #f otherwise.
             Can raise an "argument not an integer" error.
             Can raise an "argument not a list" error when applied using (apply ...).
        
        < : (< int int ...+) -> #t | #f
            Returns #t if arguments are in strictly decreasing order.
            Returns #f otherwise.
            Can raise an "argument not an integer" error.
            Can raise an "argument not a list" error when applied using (apply ...).
        
        <= : (<= int int ...+) -> #t | #f
             Returns #t if arguments are not in increasing order.
             Returns #f otherwise.
             Can raise an "argument not an integer" error.
             Can raise an "argument not a list" error when applied using (apply ...).
        
        eq? : (eq? x y) -> #t | #f
              Takes two arguments of any type.
              Returns #t if x and y have the same value.
              Returns #f otherwise.
        
        not : (not x) -> #t | #f
              Takes one argument of any type.
              Returns #t if x is #f.
              Returns #f otherwise.
        
    Types:
        number? : (number? x) -> #t | #f
                  Takes one argument of any type.
                  Returns #t if x is an int.
                  Returns #f otherwise.
        
        integer? : (integer? x) -> #t | #f
                   Takes one argument of any type.
                   Returns #t if x is an int.
                   Returns #f otherwise.
        
        cons? : (cons? x) -> #t | #f
                Takes one argument of any type.
                Returns #t if x is a pair as constructed by cons.
                Returns #f otherwise.
        
        procedure? : (procedure? x) -> #t | #f
                     Takes one argument of any type.
                     Returns #t if x is a function.
                     Returns #f otherwise.
        
        void? : (void? x) -> #t | #f
                Takes one argument of any type.
                Returns #t if x is void.
                Returns #f otherwise.
        
        null? : (null? x) -> #t | #f
                Takes one argument of any type.
                Returns #t if x is the empty list '().
                Returns #f otherwise.
    
    Pair Operations:        
        cons : (cons x y) -> (x . y)
               Takes two arguments of any type.
               Returns a pair containing these arguments in the form (x . y).
               If y is a pair or a list (e.g. (a . b)), the resulting value will have an implicit ".": (x a b).
        
        car : (car x) -> a, where x = (a . b) or x = (a b c)
              Returns the left element of a pair.
              Can raise an "argument not a pair" error.
        
        cdr : (cdr x) -> b, where x = (a . b) | (b c) where x = (a b c)
              Returns the right element of a pair.
              Can raise an "argument not a pair" error.
    
    List Operations:
        append : (append list ... x) -> list
                 Takes an argument of any type, preceded by any number of lists.
                 If supplied only lists, returns a list containing all their elements in order.
                 If the last argument is not a list, returns an "improper list" of the form (a b . c).
        
        drop : (drop list int) -> list
               Called as (drop lst x), returns a list containing the remaining elements of lst after the first
                    x elements are skipped.
               Integer argument must be less than the length of the provided list.
        
        length : (length list) -> int
                 Returns the number of elements contained in the given list.
    
    Vector Operations:
        vector : (vector x ...) -> vector
                 Takes any number of arguments of any type.
                 Returns a vector object containing the argument x's in order.
        
        make-vector : (make-vector int x) -> vector
                      Takes a non-negative integer as the first argument and any type as the second argument.
                      Called as (make-vector i x), returns a vector of length i populated entirely with the value of x.
                      Can raise an "argument not an integer" error.
                      Can raise an "argument must be non-negative" error.
        
        vector-ref : (vector-ref vector int) -> <any type>
                     Takes a vector as the first argument and a non-negative integer as the second.
                     Integer argument must be less than the length of the provided vector.
                     Called as (vector-ref vec i), returns the value stored at position i in the vector vec.
                     Can raise an "argument not a vector" error.
                     Can raise an "argument not an integer" error.
                     Can raise an "argument must be non-negative" error.
                     Can raise an "index out of bounds" error.
        
        vector-set! : (vector-set! vector int x) -> void
                      Takes a vector as the first argument, a non-negative integer as the second argument, and any type as the third.
                      Integer argument must be less than the length of the provided vector.
                      Called as (vector-set! vec i x), sets the value stored at position i in vec to be x.
                      Can raise an "argument not a vector" error.
                      Can raise an "argument not an integer" error.
                      Can raise an "argument must be non-negative" error.
                      Can raise an "index out of bounds" error.
    
    Other Operations:
        halt : (halt x) -> <any type>
               Takes one argument of any type.
               Prints the value of x to standard output, then halts execution of the program.
               Returns the value of x.
        
        print : (print x) -> void
                Takes one argument of any type.
                Prints the value of x to standard output.
        
        void : (void) -> void
               Takes no arguments.
               Returns a void value.

Runtime Errors:
    Handled:
        Division by 0
            Calling / with 0 as any argument but the first is not permitted.
            
            Test cases:
                (/ 3 0)
                
                (begin (define x 0)
                       (/ 3 x))
                
                (/ 64 2 2 0 2)
                
                (apply / (list 4 2 1 0))
                
                (begin (define x 0)
                       (/ 64 2 2 x 2))
                
                (begin (define x 0)
                       (apply / (list 4 2 1 x)))
        
        Negative Integer Argument
            Calling make-vector with a negative integer as the first argument is not permitted.
            Calling vector-ref with a negative integer as the second argument is not permitted.
            Calling vector-set! with a negative integer as the second argument is not permitted.
            
            Test cases:
                (make-vector -3 "a")
                
                (apply make-vector (list -3 "a"))
                
                (begin (define x -3)
                       (make-vector x "a"))
                
                (begin (define x -3)
                       (apply make-vector (list x "a")))
                
                (begin (define vec (vector 1 2 3))
                       (vector-ref vec -4))
                       
                (begin (define vec (vector 1 2 3))
                       (apply vector-ref (list vec -4)))
                       
                (begin (define vec (vector 1 2 3))
                       (vector-set! vec -2 7))
                       
                (begin (define vec (vector 1 2 3))
                       (apply vector-set! (list vec -2 7)))
            
            Note: There are fewer test cases for vector-ref and vector-set! because the error-handling code
                  is the same for all three operations.
        
        Vector Index Out of Bounds
            Calling vector-ref with an integer index greater than or equal to the length of the supplied vector is not permitted.
            Calling vector-set! with an integer index greater than or equal to the length of the supplied vector is not permitted.
            
            Test cases:
                (begin (define vec (vector 1 2 3))
                       (vector-ref vec 7))
                
                (begin (define vec (vector 1 2 3))
                       (apply vector-ref (list vec 7)))
                
                (begin (define vec (vector 1 2 3))
                       (define x 7)
                       (vector-ref vec x))
                
                (begin (define vec (vector 1 2 3))
                       (define x 7)
                       (apply vector-ref (list vec x)))
                
                (begin (define vec (vector 1 2 3))
                       (vector-set! vec 7 12))
                
                (begin (define vec (vector 1 2 3))
                       (apply vector-set! (list vec 7 12)))
            
            Note: There are fewer test cases for vector-set! because the error-handling code is the same for both operations.
            
    Not Handled:
        Function Given Incorrect Number of Arguments
            Note: This case is handled for some primitive operations, but not all of them.
                  This case is not handled at all for user-defined functions.
        Non-Function Value Applied
        Memory Use Cap Exceeded
        Use of Uninitialized Variable
        

Other Notes:
    The tests in the Runtime Errors section were conducted visually, as I was unable to figure out how to rewrite eval-top-level to
    output my own error messages. My testing process was as follows:
        1. Run eval-top-level with the test case and note the type of Racket error produced.
        2. Compile the test case using full-compile.
        3. Run eval-llvm with the compiled code and note the value returned (eval-llvm has been edited
           to return the error message from bin.exe in case of an error).
        4. Visually confirm the error message produced from eval-llvm is correct.
        5. Visually compare this messsage to that produced by eval-top-level to confirm they are of
           the same (or similar) type.
           
Example Output (from DrRacket):
    > (run-tests "all")
    Score on available tests (may not include release tests or private tests): 100.0%
    
    > (define e '(/ 3 0))
    > (eval-top-level e)
    "Evaluation failed:"
    (exn:fail:contract:divide-by-zero
     "/: division by zero"
     #<continuation-mark-set>)
    '(/ 3 0)
    > (eval-llvm (full-compile e))
    "library run-time error: (prim / a b); division by 0\r"
           
Academic Pledge:
    I, Cameron Smith, pledge on my honor that I have not given or received any unauthorized assistance on this assignment.
