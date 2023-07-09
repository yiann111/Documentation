test_that("Check filename accuracy",{
    expect_identical(make_filename(2013),"accident_2013.csv.bz2")
}
          )


test_that("Error consistency of fars_read()",{
    expect_error(fars_read("accident_2018.csv.bz2"))

})



