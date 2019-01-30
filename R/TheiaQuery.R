TheiaQuery <-
  R6Class("TheiaQuery",
          # public -------------------------------------------------------------
          public =
            list(query  = NULL,
                 login  = NULL,
                 passwd = NULL,

                 initialize = function(login, passwd, query)
                 {
                   self$login  <- login
                   self$passwd <- passwd

                   # analyze query
                 })
          )


