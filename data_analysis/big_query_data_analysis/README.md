# Assignment 4: BigQuery

This repository contains starter code for your fourth assignment, which will involve downloading and processing data from Google BigQuery. In other words, you will be accessing a relational (SQL) database from your R sessions.

The goal of the assignment is replicate Figure 3 from [McDermott et al. (2018)](https://grantmcdermott.com/papers/mcdermott2019blueparadox.pdf) using publicly available GFW data. I've already written out a bunch of code to help get you on your way. I've also written out the code for the actual figure right at the end, since that gets a little complicated and it's not my goal to test advanced plotting skills in this assignment.

The structure of the newly-cloned repository is as follows:

```
repo
|
-- README.md
|
-- .gitignore
|
-- data
   |
   -- (various files)
|
-- paper
   |
   -- mcdermott2018.pdf
|
-- pics
   |
   -- (two files)
|
-- R
   |
   -- gfw.Rmd

```

You should clone this repo using the RStudio Project method that we practiced in class. Then, I want you to open up the `R/gfw.Rmd` file where you will find the questions and basic template for providing your answers. You should answer each question by inserting R code chunks (like we practiced in class) in the relevant spaces. Whenever you "knit" this `R/gfw.Rmd` file, you will produce the corresponding `R/gfw.md` (i.e. Markdown) file, as well as some auxilliary folders. Don't worry about those now; they are just to help speed things up and my template should take care of everything for you.

Last things: Please pay attention when I ask you to comment on your results in addition to coding them up. **And please mark your comment answers in bold so that I can easily identify them.**

**Grade: B+**
