# monad-par-async

This package provides a lightweight `Async` implementation which is a thin wrapper around `ParIO` of the `monad-par` package. Pros and cons are listed below:

Pros:

* Support exception handling, compared to `ParIO`. Exceptions will not be silently dropped, and `MonadThrow`/`MonadCatch` instances are provided.
* More lightweight than `Async` in `async`, since the latter forks a thread per task.
* `Applicative` instance performs two tasks concurrently.

Cons:

* No support for killing a task.
* No support for using with STM.
