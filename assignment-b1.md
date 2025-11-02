Assignment B1
================
Emma Collins

## Exercise 1 and 2

In this exercise, I will create the function parity() that takes in a
numerical integer and returns odd or even based on the parity of the
integer. This can be done by looking at the remainder when dividing the
value by 2. If the remainder is 0 then the value is even and otherwise
the value is odd. One consideration for this function is what should
occur if *NA* values are input into the function. When this occurs I
want my function to output *NA*. There are a few errors that I want to
account for in this function. One error case is if the input is not a
numeric and the other is if the numeric does not contain only integers.
In these cases, I want the function to throw an error. In the code block
below, you can find the description and function code.

``` r
#'@title Find Parity of Integer
#'
#'@description This function finds the parity (even or odd) of a given integer value or a numerical vector that contains only integers
#'
#'@param x integer, vector
#'@returns character, vector containing even or odd depending on the parity of the input
#'
#'@example parity(12)
#'@example parity(c(-5,2,4,7))
parity <- function(x) {
  output<- rep(NA, length(x))
  #throw this error message and stop the function if x is not a numerical vector
  if(!is.numeric(x)){
      stop("x must be a numerical vector that contains only integers") 
    }
  #checking if all values in x are integers
  if(suppressWarnings(all(x%% 1 != 0,na.rm = TRUE))){
      stop("x must be a vector of length greater than 0 that contains only integers") #throw this error message and stop the function
    }
  for(i in 1:length(x)){
    if(is.na(x[i])){
      output[i] = NA
    }
    else if(x[i] %% 2 == 0){
    output[i] = "even"
    }
    else{output[i] = "odd"}
    }
  return(output)
}
```

## Exercise 3

In this portion of the assignment, I will include some examples uses of
the *parity* function. In the following code block, we will look at
three simple examples. One containing one singular positive integer
value and the other example containing a vector of a few positive
integers and 0. The last example will use the parity function on a
vector that contains negative and positive integers (to demonstrate that
it still works with negative integers).

``` r
parity(1509)
```

    ## [1] "odd"

``` r
parity(c(0,5,253))
```

    ## [1] "even" "odd"  "odd"

``` r
parity(c(-125,-52,6,11,196))
```

    ## [1] "odd"  "even" "even" "odd"  "even"

In the creation of this function, a case was made for if the vector
contains *NA* values. If the value input is *NA* then the function will
output NA for that value. Below is an example of how the parity function
handles NA values.

``` r
parity(c(12,4,57,NA,23))
```

    ## [1] "even" "even" "odd"  NA     "odd"

In the next code block, there will be examples to demonstrate what will
occur if the input for the function is a vector that contains character
values. This should lead to an error as this function only produces
output if the in put is a numeric that contains only integers.

``` r
parity(c("12","hello","this should fail"))
```

    ## Error in parity(c("12", "hello", "this should fail")): x must be a numerical vector that contains only integers

Two other possible errors are if the numeric contains values that are
not integers and if the numeric is of length 0. In the code block below,
I will demonstrate the error message that will occur if non-integer
values are input into the function and if the length of the numeric is
0.

``` r
parity(c(12.5,8.5,6.19))
```

    ## Error in parity(c(12.5, 8.5, 6.19)): x must be a vector of length greater than 0 that contains only integers

``` r
parity(numeric(0))
```

    ## Error in parity(numeric(0)): x must be a vector of length greater than 0 that contains only integers

## Exercise 4

In this exercise, the *testthat* package will be used to make formal
tests of the function. A few tests that I will include are a singular
integer value, a numeric vector of integers, a vector that includes *NA*
values, a numeric of length 0 and a character vector. The last two test
should throw errors.

``` r
#testing values that should run
test_that("parity_run",{
  expect_equal(parity(1248678),"even")
  expect_equal(parity(c(-5,-2,28,30)),c("odd","even","even","even"))
  expect_equal(parity(c(1,4,NA,5,NA)),c("odd","even",NA,"odd",NA))
})
```

    ## Test passed 🎉

``` r
#Testing the values that should cause errors
test_that("parity_errors",{
  expect_error(parity(numeric(0)))
  expect_error(parity(c("Hello","World")))
  expect_error(parity(TRUE))
})
```

    ## Test passed 🎉

All the formal tests passed!
