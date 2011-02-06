;;; Copyright (c) B. Smith-Mannschott. All rights reserved.
;;; The use and distribution terms for this software are covered by
;;; the Eclipse Public License 1.0
;;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;;; found in the file LICENSE at the root of this distribution.
;;; By using this software in any fashion, you are agreeing to be
;;; bound by the terms of this license.  You must not remove this
;;; notice, or any other, from this software.

(ns bpsmithmannschott.adiutores.parva)

(defmacro dofor
  "A synonym for doseq."
  [& more]
  `(doseq ~@more))

(defmacro domap
  "Equivalent to (dorun (apply map f colls))."
  [f & colls]
  {:pre [(seq colls)]}
  `(dorun (map ~f ~@colls)))

(defmacro forcat
  "For's analog of mapcat. Equivalent to (apply concat (for [...] ...))."
  [seq-exprs body-expr]
  `(apply concat (for ~seq-exprs ~body-expr)))

(defmacro let1
  "Like let, but for a single binding. Since the number of bindings is
not variable, we don't group them in a vector.

example: (let1 pi 3.14159
            (* pi pi)) => 9.869587728099999

(let1 name value body) is equivalent to (let [name value] body).
Since it is built on let, 'name' may actually be a destructuring,
though it's better style to use standard let if you're going to
destructure."
  [bindable-form init-expression & body]
  `(let [~bindable-form ~init-expression]
     ~@body))

(defmacro thread-with
  "Similar to the -> and ->> macros in clojure.core, but more flexible because
one specifies the place where the previous expression is to be inserted
by using varname at that spot.

usage> (thread-with x 2
           (inc x)
           (/ x 2)) => 3/2
This particular example is equivalent to: (-> 2 inc (/ 2))
or (/ (inc 2) 2).

Naming the variable gives us control over where we use the value of
the previous expression. In fact, we can even use the value of the
previous expression more than once:

usage> (thread-with x 3
           (* x x)) => 9

So, really, thread-with is equivalent to a let which repeatedly binds
the same name:

  (thread-with x 5
    (/ 15 x)
    (* x x)
    (/ x 2)) => 9/2

is equivalent to:

  (let [x 5
        x (/ 15 x)
        x (* x x)]
    (/ x 2))

But with less repetition."
  [varname & expressions]
  {:pre [(symbol? varname) (not (namespace varname))]}
  `(let [~@(interleave (repeat varname) (drop-last expressions))]
     ~(last expressions)))
