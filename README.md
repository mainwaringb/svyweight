# Rakehelper
Quick and Flexible Rake Weighting Package for R

Ben Mainwaring

## Description
R package for quickly and flexibly calculating rake weights (also known as rim weights, 
or iterative proportional fitting). This allows post-stratification/non-response
weighting on multiple variables, even the interlocked distribution of the two
variables is not known. Interacts with Thomas Lumley's [survey](http://r-survey.r-forge.r-project.org/survey/) package, 
and adds additional functionality, more adaptable syntax, and error-checking
to the weighting functionality in survey.

The core function in Rakehelper is `rakesvy` (and the related `rakew8`), which calculates post-stratification weights for a dataset or svydesign object, given targets. The command is designed to make weighting as simple as possible, with the following features:
- Weighting to either counts or percentage targets
- Allowing specification of targets as vectors, matrices, or data frames
- Accepting targets of 0 (equivalent to dropping cases from analysis)
- Allowing targets to be quickly rebased to a specified sample size
- Flexibly matching targets to the correct variables in a dataset
- Dynamically specifying weight targets based on recodes of variables in observed data

More details about the package are available in the R help files (see `package?Rakehelper` in R).

## Planned Features
The package is under development, and additional features are planned for future release. This includes:
- Allowing targets with NA values on some categories (in progress)
- Techniques for weighting numeric and ordinal data based on histograms/binning
- Generalising GREG weighting to random forests or other models that allow for nonlinear relationships between variables


Contributions to the package, or suggestions for additional features, are gratefully accepted via email or GitHub. 



