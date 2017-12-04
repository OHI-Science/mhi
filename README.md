# OHI gh-pages branch

The MHI website is available at <http://ohi-science.org/mhi>.

If that site is down, it is also available [here](https://rawgit.com/OHI-Science/mhi/gh-pages/layers.html).

To build the website, run [`rmarkdown::render_site()`](http://rmarkdown.rstudio.com/rmarkdown_websites.html), which knits each Rmarkdown (*.Rmd) based on parameters in `_site.R` and `_site.yml` before pushing the files back to Github. These Rmd files also pull from the `data_branch`, typically `draft`, that gets copied into `[gh_repo]_[gh_branch_data]`. 

The majority of this website architecture was coded by [Ben Best](https://github.com/bbest) with significant inputs from [Julie Lowndes](https://github.com/jules32).
