Untitled
================

## Introduction

This exercise is typically solved using Hashmaps. However, these are not
available in base R. So to start, I decided to use this as a chance to
add solutions to the already-exhaustive list.

## Problem

Given an array of integers, return indices of the n numbers such that
they add up to a specific target. <BR> Assumptions: 1. Each input has
exactly one solution <BR> 2. You may not use the same element twice.

``` r
given.nums <- c(2, 8, 9, 10, 14, 15, 18, 22)
target.num <- 29
```

## Solution 1: Combinations of indices

This works okay for smaller arrays, but will be pretty inefficient with
larger ones. Importantly, the question assumes that the input array only
offers <i>one</i> solution. This allows us to use a simple ‘while’ loop
and stop when the index combination is
found.

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
  
  ### Extra for educational purposes
  ret.inds <- combo.mat[, i]
  inds.str <- paste(ret.inds, collapse = ', ')
  nums.str <- paste(given.nums[ret.inds], collapse = ', ')
  
  ret.text <- sprintf("Adding %s from indices %s returns the target value of %d", 
                      nums.str, inds.str, target_num)
  
  return(cat(ret.text)) # return indices
  
}
```

Now let’s test the function for different lengths of combinations to get
our target number of
    29.

``` r
n_sum(given_nums = given.nums, target_num = target.num, comb_len = 2)
```

    ## Adding 14, 15 from indices 5, 6 returns the target value of 29

``` r
n_sum(given_nums = given.nums, target_num = target.num, comb_len = 3)
```

    ## Adding 2, 9, 18 from indices 1, 3, 7 returns the target value of 29

``` r
n_sum(given_nums = given.nums, target_num = target.num, comb_len = 4)
```

    ## Adding 2, 8, 9, 10 from indices 1, 2, 3, 4 returns the target value of 29

The last few lines can be changed to `return(combo.mat[, i])` but a full
text return may help for educational purposes.

## Solution 2: Hashmap alternative

Coming soon…
