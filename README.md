# bluebell-utils

Jonas's work-in-progress utility library

## What's inside?

- [bluebell.utils.dsl](src/bluebell/utils/dsl.clj)
  A tool to build DSLs explained [here](http://ostlund.rocks/posts/2018-09-06-dsls/).
- [bluebell.utils.render-test](src/bluebell/utils/render_text.clj)
  A DSL to render text with indentation.
- [bluebell.utils.ebmd](src/bluebell/utils/ebmd.clj)
  Example-based multiple dispatch explained [here](http://ostlund.rocks/posts/2018-09-14-ebmd/).

## More docs

### `bluebell.utils.error-context`

An error context is an, ehmm, mutable object to help with the control flow in case of error handling. It is normally constructed using the context function `context`. Internally, it tracks whether errors occur and, if that is the case, prevent computations from taking place.

It is practical for factoring out error-handling business and does not need macros to hide what is going on:
```clj
;; We can factor out the details about errors...
(defn stacked-ex-context []
  (-> (context)
      (maperr-when (partial instance? Exception)
                   (constantly :exception))
      (maperr-when (partial instance? ArithmeticException)
                   (constantly :arithmetic-exception))))



;; ..and then reuse it.
(deftest maperr-when-test
  (is (= 7 (with-export (stacked-ex-context)

                      7)))
  (is (= :exception (with-export (stacked-ex-context)

                               (throw (ex-info "Kattskit" {})))))

  (is (= :arithmetic-exception (with-export (stacked-ex-context)

                                          (/ 8 0)))))
```

See the unittests `bluebell.utils.error-context-test` for examples.

Why *error context* over exceptions?:
    * Makes error handling more explicit by requiring the use of a context to control execution based on whether an error occurred, without cluttering the code with `if-let` forms.
    * Isolate error handling to parts of the code and then wrap it up in the end, with a call to `export`.
    * Handle situations where it would be a bit brutal to throw an exception, e.g. situations such as `spec/conform` returning `spec/invalid`.
    * Factor out the details about error handling in functions.


## License

Copyright © 2018 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
