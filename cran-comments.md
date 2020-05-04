## Re-submission Date: 2020-05-03
## Original Submission Date: 2020-04-26

## Test environments

* local Windows 10 install, R 3.6.0, 4.0.0 and 4.1.0 (development version)
* local ubuntu 18.04 install, R 3.6.2
* R-hub builder
* win-builder (oldrelease, release, devel)

## Re-submission changes

* Comment 01:
```
Please add more details about the package functionality and implemented
methods in your Description text.
```

Additional details about the package functionality and methods have been
added to Description.

* Comment 02:
```
\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or create
additionally small toy examples to allow automatic testing.
(You could also replace \dontrun{} with \donttest, if it takes longer
than 5 sec to be executed, but it would be preferable to have automatic
checks for functions. Otherwise, you can also write some tests.)
```

Examples that were wrapped in \dontrun{} have all been replaced with toy
examples that are executable in < 5 sec. Notes have been added as comments
for user to know that the examples are small for illustration purpose only.

* Additional changes

Package version has been changed to 0.1.0 to reflect the fact that this is
a beta release. 

## R CMD check results
There were no ERRORs, or WARNINGs. There were 01 NOTE. 

* NOTE 01:
```
> checking CRAN incoming feasibility ... NOTE
Maintainer: 'Minh Trinh <mdtrinh@mit.edu>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Yamamoto (14:13)
  Zhou (14:4)
  ignorability (12:41)
```
    
Confirming that this is a new submission. Yamamoto and Zhou are proper names of package co-authors. "ignorability" is a concept in causal inference.
