# Scheme, CL, and FP notes

Constant/'variable'
```
(define a
  (* 5 5))
```

Procedure (scheme)
```
(define (name arg1 arg2 . argn)
  (blah blah blah))
```

Everything after the '.' in `(name arg . argn)` is an optional param, and is
of variable length. argn is returned as a list.

Procedure (common lisp)
```
(defun name (arg1 arg2 &rest argn)
  (blah blah blah))
```

Procedure with no arguments... just give it no arguments
```
(define (b)
  (* 5 5))
```

evaluating `b` returns `#<procedure:b>` (literal symbol)
evaluating `(b)` returns 25

Procedure with no arguments (above desugared)
```
(define c
    (lambda () (* 5 5)))
```

Procedure with no name and no arguments? just `(lambda () ...)`

## Scheme functions

- begin

- set!

- eq? eqv? equal?

- number? zero? pair? symbol? null?

- display write newline

- (list-tail lst n) (list-ref lst n)

- subset?

- map

- for-each (has side effects, like a map using set!)

- vector-ref vector-set!

- substring

- \#t #f

(aside)

Scoping
-------

1. Lexical scope/static scope - references are only possible within the construct
in which the entity is defined.

2. Dynamic scope - references are possible globally

Scheme has lexical scope only, CL has both.

Lambda in CL
------------

Lambda is just a symbol. Only special when it's the car of the list, which turns
the list into a lambda form. But it's still just a list: the (function ...) form
turns the lambda form into a function. Lambda is further defined as a macro,
which automatically applies the function form to a lambda form.

So,

`'(foo lambda bar)` is just a list

`(lambda (x) (+ 1 x))` is a lambda form, but still just a list

-> converted into a function

`(function (lambda (x) (+ 1 x)))`

-> using the #' shorthand, the two are equivalent

`#'(lambda (x) (+ 1 x))`

-> but, due to the lambda macro, lambda forms are actually functions
so we can just write

`(lambda (x) (+ 1 x))`

which the macro system expands automatically to

`#'(lambda (x) (+ 1 x))`

Further, in common lisp, variables and functions are in different namespaces.
Functions are named in the function namespace, but variables that point to
functions are identified as variables in its own namespace. So to use such a
variable, we must first evaluate it as a function form with `(function ...)` or `#'`.

example:

`(defun x ...)` and `(setf y (function x))`

`x` is a function, `y` is a variable pointing to a function.
`(eq x y)` will return an error, trying to reference `x` in variable namespace as a function.
but `(eq #'x y)` is true, both `#'x` and `y` evaluate to the same function in memory.
(end of aside)

## Closures/object systems (from Let over Lambda)

> But if I were trying to solve the problem one reader sent to me as a canonical
> example of an OO problem, I wouldn't.
>
> > "Suppose you have n serial ports, each of which may speak one of k protocols, and
> > this must be configurable at run-time."
>
> I'd just use an n-by-k array of closures to represent this.
> -- Paul Graham

### What is a closure? What is an object?
Objects can be built from "assignable value cells" and lambdas (Steele)

assignable value cell - an environment storing pointers to data with indefinite
extent (env is persistent)

```
(define (env-indefinite input)
    (cons input nil))
```

### Let over lambda - a closure (or an object)
```
(let ((counter 0))
    (lambda () (incf counter)))
```
x is a free variable closed over the let form. Evaluating the let returns a
function carrying an environment. Because of indefinite extent this
environment is persistent. A closure is a single function with storage, or an
object with one method. Returning multiple lambdas = "multiple methods".

```
(let ((counter 0))
  (values
    (lambda () (incf counter))
    (lambda () (decf counter))))
```
If we used defun inside let, then we will give the functions dynamic scope but
the variable lexical scope. This isn't possible in scheme, but can be simulated
by using define-values, which evals the expr then binds the results to the ids
you give it.

```
(define-values (my-incf my-decf)
  (let ((x 0))
    (values (lambda (y)
              (set! x (+ x y))
              x)
            (lambda (y)
              (set! x (- x y))
              x))))
```

or to improve readability

```
(define-values (my-incf my-decf)
  (let ((x 0))
    (define (my-incf y)
      (set! x (+ x y))
      x)
    (define (my-decf y)
      (set! x (- x y))
      x)
    (values my-incf my-decf)))
```

### Lambda over let over lambda - a class (an object creating thing)
Can be an anonymous class (use a lambda), or a named class with define.

Evaluating this will return a new closure, each with its own state variables.

```
(lambda ()
  (let ((x 0))
    (lambda ()
      (+ x 1))))
```

The above form returns a pointer to a list. When executed, the let form in the
list creates an environment at runtime, inside of which contains yet another
pointer to more machine code, where the free variable x is bound in the
environment.

Remember that compiled lambdas are a constant form, pointers to machine code.
Closures create new envs at runtime, so hence cannot be compiled. But if you
compile a closure returning function, then the closures it returns will be
compiled also.

### Let over lambda over let over lambda
Same thing, with "static variables" closed over the class.
