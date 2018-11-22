# A quick to do list

Off the top of my head

- choose best output format: df with x and y coordinates as columns seems maybe the most intuitive?
- document unit of radius?
- sensible choices for the default parameters, e.g.
  - if `toLabel` not provided, defaults to TRUE for all points provided
  - radius which does behave more or less nicely 
- input checks: x and y same length, and numeric
- check the behaviour itself of the function in cases where the labels would be a lot
- wishlist: some anchor segments to refer to the points?
- maybe encapsulate the whole thing into a label + segments plotter?

- if it gets full, well, package up
  - If so, a vignette would be very nice
  - or at least, a README with outputs - for that, meaningful way would be to use README.Rmd and render that to github-md
- check what the behaviour of ggrepel is in edge cases?

 