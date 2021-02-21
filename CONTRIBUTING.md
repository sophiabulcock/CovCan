The contributions to our project and workflow will be as follows:

Steps:

1. Set up GitHub repo, decided and get approval on the API we choose to use.

- Got approval for API that gets current and historical COVID-19 counts in Canada by province

2. Set up workflow, decide who makes which functions. Aim to have this done February 8th/9th.

Yuxian:

  - Build function to retrieve data current and historical data, output into a useable data frame
  - Build function that returns a map of Canada with the selected statistics for infected or deceased. This will be a visual map

Graham:

  - Help Yuxian build function mentioned above
  - Build function that calculates the daily new cases and death counts from the pre-existing information from the data frame function Yuxuan made, and adds it to the data frame to produce a new data frame

Sophia:

  - Build 4 data visualization functions that use both the data frames mentioned above.
    - View current COVID-19 data for any combination of Canadian regions and date
    - View a time series plot between 2 dates for any combination of regions that plots infected count and deceased count over time
    - View a bar chart with change in infected and deceased counts between 2 days
    - View a bar chart on a selected date that plots the number of NEW people infected and deceased from COVID-19, a specified region of Canada

3. Each member will then create test functions each function they made in a package, using RUnit package.
4. Each member will add a docstring to each of the functions they were responsible for.
5. A specified member will make a test suite function to test that all the test functions work as expected

  - Sophia did this

6. Set up package the way we want, appropriately laid out for a test build
7. Yuxian will test to see if the build works, with Sophia and Graham on video chat
8. Each member will then contribute evenly to building the package vignette.
9. Submit package
