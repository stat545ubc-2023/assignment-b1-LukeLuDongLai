Assignment B-1: Making a function
================

This assignment covers making a function in R, documenting it, and
testing it.

# Setup

Begin by loading the data and the packages that will be used:

``` r
library(dplyr)
library(testthat)
library(palmerpenguins) # <- contain the test dataset
```

# Exercise 1 & 2: Make a Function & Document your Function

Let’s create a function that simplifies a common data analysis task in
R, particularly one involving *dplyr* for data manipulation. I’ll create
a function called *summarize_by_group* which will group a given
dataframe by a specified column and then summarize another specified
numeric column, calculating *mean*, *median*, and *two quartiles (first
and third quartiles)*.

**Justification for naming**: The chosen parameter names in the
*summarize_by_group* function — *data*, *group_col*, and *summary_col* —
are straightforward and descriptive. *data* specifies the dataframe to
be analyzed. *group_col* intuitively denotes the column name used for
data grouping, directly combining the action (grouping) with its target
(column). Similarly, *summary_col* clearly specifies the column name
used for summarizing, indicating both the action (summary) and the data
element (column) it applies to.

``` r
#' Summarize a Numeric Column by Group
#'
#'This function groups the data by a specified column and then calculates the mean, median, and two quartiles (first and third quartiles) for a specified numeric column.
#' @param data A dataframe containing the data to be summarized.
#' @param group_col A character string of a column name by which data is to be grouped.
#' @param summary_col A character string of a numeric column name to summarize.
#'
#' @return A dataframe with each group and the mean, median, and Q1 and Q3 of a specified numeric column.

summarize_by_group <- function(data, group_col, summary_col) {
  # Check if data is a dataframe or a tibble
  if (!is.data.frame(data)) {
    stop("'data' must be a dataframe or a tibble.")
  }
  # Check if group_col and summary_col are single character variables
  if (!is.character(group_col) || length(group_col) != 1) {
    stop("'group_col' must be a single character string.")
  }
   if (!is.character(summary_col) || length(summary_col) != 1) {
    stop("'summary_col' must be a single character string.")
  }

  # Convert group_col and summary_col to symbol
  group_col <- rlang::syms(group_col)
  summary_col <- rlang::syms(summary_col)
  
  data %>%
    group_by(!!!group_col) %>%
    summarise(across(!!!summary_col, list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      Q1 = ~quantile(., probs = .25, na.rm = TRUE),
      Q3 = ~quantile(., probs = .75, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) %>%
    ungroup() # Ensure that subsequent operations are not affected by the original grouping
}
```

# Exercise 3: Include examples

Let’s take *penguins* dataset for example.

``` r
head(penguins)
```

    ## # A tibble: 6 × 8
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           NA            NA                  NA          NA
    ## 5 Adelie  Torgersen           36.7          19.3               193        3450
    ## 6 Adelie  Torgersen           39.3          20.6               190        3650
    ## # ℹ 2 more variables: sex <fct>, year <int>

**Example 1 (Proper Usage): Summarizing by a Grouping Column**

``` r
# Summarizing 'bill_length_mm' by 'island' 
summary_1 <- summarize_by_group(penguins, "island", "bill_length_mm")

print(summary_1)
```

    ## # A tibble: 3 × 5
    ##   island    bill_length_mm_mean bill_length_mm_median bill_length_mm_Q1
    ##   <fct>                   <dbl>                 <dbl>             <dbl>
    ## 1 Biscoe                   45.3                  45.8              42  
    ## 2 Dream                    44.2                  44.7              39.2
    ## 3 Torgersen                39.0                  38.9              36.7
    ## # ℹ 1 more variable: bill_length_mm_Q3 <dbl>

**Example 2 (Improper Usage): Summarizing by Multiple Grouping Columns**

``` r
# Summarizing 'bill_length_mm' by both 'island' and 'sex'
summary_2 <- summarize_by_group(penguins, c("island", "sex"), "bill_length_mm")
```

    ## Error in summarize_by_group(penguins, c("island", "sex"), "bill_length_mm"): 'group_col' must be a single character string.

**Example 3 (Improper Usage): Summarizing Multiple Columns by a Grouping
Column**

``` r
# Summarizing both 'bill_length_mm' and 'body_mass_g' by 'species'
summary_3 <- summarize_by_group(penguins, "species", c("bill_length_mm", "body_mass_g"))
```

    ## Error in summarize_by_group(penguins, "species", c("bill_length_mm", "body_mass_g")): 'summary_col' must be a single character string.

**Example 4 (Improper Usage): Inputting Dataset of the Wrong Type**

``` r
# Making an arbitrary list
list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)

summary_4 <- summarize_by_group(list_data, "Red", "Green")
```

    ## Error in summarize_by_group(list_data, "Red", "Green"): 'data' must be a dataframe or a tibble.

# Exercise 4: Test the Function

In this section, I will use *testthat* package to wrap all the test
examples from **Exercise 3**. Since these examples demonstrate both
correct usage of the function and common errors with different types of
incorrect inputs, I consider them non-redundant.

``` r
test_that("Test summarize_by_group function", {
  
  # Correct Usage
  expect_s3_class(summarize_by_group(penguins, "species", "body_mass_g"), "data.frame")
  
  # Summarizing by Multiple Grouping Columns
  expect_error(summarize_by_group(penguins, c("island", "sex"), "bill_length_mm"), "'group_col' must be a single character string.")
  
  # Summarizing Multiple Columns by a Grouping Column
  expect_error(summarize_by_group(penguins, "species", c("bill_length_mm", "body_mass_g")), "'summary_col' must be a single character string.")
  
  # Inputting Dataset of the Wrong Type
  expect_error(summarize_by_group(c(1:4), "2", "3"), "'data' must be a dataframe or a tibble.")
  
  # Non-character Input
  # Consider a scenario where a column name is composed entirely of numbers, and the user inputs numeric values instead of a character string
  test_dataframe <- data.frame( '1' = letters[1:5], '2' = 1:5)
  expect_error(summarize_by_group(test_dataframe, 1, 2), "'group_col' must be a single character string.")
})
```

    ## Test passed 🥇
