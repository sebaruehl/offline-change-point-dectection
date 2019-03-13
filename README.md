# Offline Change Point Detection
Very basic offline change point detection based on bootstrapping written in R.

This implementation serves more an educational purpose than a well defined framework for change point detection in time series. There exist already packages written in R which provide sophisticated methods for this problem case, e.g. the 'changepoint' package available on [cran](https://cran.r-project.org/web/packages/changepoint/changepoint.pdf).

The implemented functions were developed as part of a Bachelor thesis written about event detection in tagged art data. The thesis is available online at the Teaching and Research Unit Programming and Modelling Languages of LMU Munich and can be found [here](https://www.en.pms.ifi.lmu.de/publications/index.php) (direct [link](https://www.en.pms.ifi.lmu.de/publications/projektarbeiten/Sebastian.Ruehl/PA_Sebastian.Ruehl.pdf) to the document).

## Planned improvements
- rework code to use functions which are part of R
- use R times series object
- add examples

## Theory

Basic outline, 2 parts of algorithm:
1. Basic offline changepoint detection on given timeseries based on CUSUM for detecting changes points in mean or variance for assumed distribution.
2. Determination of confidence of change point using bootstrapping.


## How to use
TODO

## Example
TODO
