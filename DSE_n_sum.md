Two/Three/Four Sum
================

## Introduction

This exercise is typically solved using Hashmaps. However, there is no
native support for these in R. So here are some workarounds from
scratch.

## Problem

Given an array of integers, return indices of the n numbers such that
they add up to a specific target. <BR> Assumptions: <BR> 1. Each input
has exactly one solution <BR> 2. You may not use the same element twice.

``` r
given.nums <- c(2, 8, 9, 10, 14, 15, 18, 22, 14)
target.num <- 29
```

## Solution 1: Combinations of indices

Solution 1 uses a ‘while’ loop to generate all possible combinations,
then loop through possibilities until it finds the first set of
indices.

``` r
# inputs: vector of numbers, target output number, and length of combination

n_sum <- function(given_nums, target_num, comb_len){
  
  ### Preliminary values
  combo.vec <- seq_len(length(given_nums)) # vector of numbers from 1 to length of input
  combo.mat <- combn(combo.vec, comb_len) # matrix of all possible index combinations
  
  i <- 1
  
  ### Adds numbers in each column until the target combo is found
  while(sum(given.nums[c(combo.mat[, i])]) != target_num){
    i <- i + 1
    next
  }
  
  return(combo.mat[, i]) # return indices
  
}
```

And to test the
function:

``` r
twosum <- n_sum(given_nums = given.nums, target_num = target.num, comb_len = 2)
threesum <- n_sum(given_nums = given.nums, target_num = target.num, comb_len = 3)
foursum <- n_sum(given_nums = given.nums, target_num = target.num, comb_len = 4)
list('2sum' = twosum, '3sum' = threesum, '4sum' = foursum)
```

    ## $`2sum`
    ## [1] 5 6
    ## 
    ## $`3sum`
    ## [1] 1 3 7
    ## 
    ## $`4sum`
    ## [1] 1 2 3 4

If the problem stated that all combinations must be captured, we can
easily accomplish this by changing the <i>while</i> loop to a slightly
modified <i>for</i> loop. Alternatively, we can apply a function to get
all combinations.

### Solution 2: apply a function

Solution 2 addresses the two-sum problem without the assumption of
having a single solution for the input array. This is done by applying a
function to a matrix of all possible index combinations, then returning
<i>all</i> combinations as a vector or matrix.

``` r
n_sum2 <- function(given_nums, target_num, comb_len){

  ### Preliminary values
  combo.vec <- seq_len(length(given_nums)) # vector of numbers from 1 to length of input
  combo.mat <- combn(combo.vec, comb_len) # matrix of all possible index combinations
  
  ### Make function that determines if sum is the target number
  sum_equals <- function(x, y){ sum(x) == y } 
  
  ### Find which indices to keep by applying sum_equals on sets of indices
  keepinds <- apply(combo.mat, 2, function(inds) { 
    sum_equals(given_nums[inds], target_num)
    }) 
  
  return(combo.mat[,keepinds])
  
}
```

Again testing the
function:

``` r
twosum <- n_sum2(given_nums = given.nums, target_num = target.num, comb_len = 2)
threesum <- n_sum2(given_nums = given.nums, target_num = target.num, comb_len = 3)
foursum <- n_sum2(given_nums = given.nums, target_num = target.num, comb_len = 4)
list('2sum' = twosum, '3sum' = threesum, '4sum' = foursum)
```

    ## $`2sum`
    ##      [,1] [,2]
    ## [1,]    5    6
    ## [2,]    6    9
    ## 
    ## $`3sum`
    ## [1] 1 3 7
    ## 
    ## $`4sum`
    ## [1] 1 2 3 4
