## Load packages
library(rofi)

noacsr::source_all_functions()

## Prepare data
prepared.data <- prepare.data()

##clean data
cleaned.data <- clean.dataset(prepared.data)

## Create subset 2
factors.data2 <- create.factors2(cleaned.data)

## table1
table1 <- table.1(factors.data2)

## result1
result1 <- result.1(factors.data2)

## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
