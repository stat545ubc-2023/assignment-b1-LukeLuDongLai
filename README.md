# Overview
This repository contains materials for the "Assignment B-1: Making a function" task, focused on R function creation, documentation, and testing.

# Contents
**R Markdown File (\*.Rmd)**: Details the development of the *summarize_by_group* function in R, its documentation, and usage examples.

**Markdown File (\*.md)**: Generated from the .Rmd file, provides an accessible format of the assignment.

# Function
**summarize_by_group**:

Groups the data by a specified column and then calculates the mean, median, and two quartiles (first and third quartiles) for a specified numeric column.

# Usage
Setup: 'dplyr' package should be loaded.

Examples: summarize_by_group(penguins, "species", "body_mass_g")

Feel free to explore and test the function!
