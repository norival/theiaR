context("Test 'TheiaCart' objects'")


test_that("'TheiaCart' objects are correct",
          {
            cart.file <- "dummy_cart.meta4"
            dest.dir  <- tempdir()

            expect_warning(cart <- TheiaCart(cart.file, dest.dir))
            expect_s4_class(cart, "TheiaCart")

            expect_is(cart@cart.infos$pub.date, "POSIXct")
          })
