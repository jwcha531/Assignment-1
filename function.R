## Exercice 3-a
# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL

sum_column <- function(d, var) {
  result <- 0
  x <- d[[var]]
  if (is.numeric(x)) {
    for (number in x) {
      result <- result + number
    }
    return(result)
  }
  return(NULL)
}

## [1] 876.5
print(sum_column(iris, "Sepal.Length"))
# NULL
print(sum_column(iris, "Species"))
# [1] 1520
print(sum_column(warpbreaks, "breaks"))


## Exercice 3-b
# Sum values in a vector.
#
# ARGUMENTS :
# x: a vector
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
my_sum <- function(x) {
  result <- 0
  if (is.numeric(x)) {
    for (number in x) {
      result <- result + number
    }
    return(result)
  }
  return(NULL)
}

## [1]876.5
print(my_sum(iris$Sepal.Length))
## NULL
print(my_sum(iris$Species))
## [1]1520
print(my_sum(warpbreaks$breaks))


## Exercice 3-c
# Sum values in a vecotr divided by a number
#
# 2 ARGUMENTS:
# x: a vector
# k: a number
#
# RETURN VALUE:
# It retruns the sum of the elements of x divided by number k.
# If either x or k ar non-numeric, it should retrun NULL.
sum_divided_by <- function(x, k){
  # Verifies if x and k are numerics
  if(!is.numeric(x)||!is.numeric(k)){return(NULL)}
  # Calculates the sum of numbers in x and divides it by k
  return(my_sum(x)/k)
}

## 73.04167
print(sum_divided_by(iris$Sepal.Length, 12))
## NULL
print(sum_divided_by(iris$Species, 22))
## NULL
print(sum_divided_by(iris$Sepal.Length, "Not numeric"))
## [1] -126.6667
print(sum_divided_by(warpbreaks$breaks, -12))


## Exercice 3-d
# Mean of a vector
#
# ARGUMENTS:
# X: a vector
# RETURN VALUE:
# If x is a non-numeric vector in should retrun NULL
my_mean <- function(x){
  # Calculates the sum of numbers in x and divides it by its length
  if(!is.null(my_sum(x))){
    return(my_sum(x)/length(x))
  } else {
    return(NULL)
  }
}

## [1] 5.843333
print(my_mean(iris$Sepal.Length))
## NULL
print(my_mean(iris$Species))
## [1] 28.14815
print(my_mean(warpbreaks$breaks))
