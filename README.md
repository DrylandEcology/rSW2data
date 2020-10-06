
| Unix | Windows | Release | License | Coverage |
| :---- | :---- | :---- | :---- | :---- |
[ ![Travis build status][1]][2] | [![Appveyor build status][3]][4] | [ ![github release][5]][6] | [![license][7]][8] | [![codecov status][9]][10] |

[1]: https://travis-ci.org/DrylandEcology/rSW2data.svg?branch=master
[2]: https://travis-ci.org/DrylandEcology/rSW2data
[3]: https://ci.appveyor.com/api/projects/status/8flt0gx8q1uupdqb/branch/master?svg=true
[4]: https://ci.appveyor.com/project/DrylandEcologyGit/rSW2data
[5]: https://img.shields.io/github/release/DrylandEcology/rSW2data.svg?label=current+release
[6]: https://github.com/DrylandEcology/rSW2data/releases
[7]: https://img.shields.io/github/license/DrylandEcology/rSW2data.svg
[8]: https://www.gnu.org/licenses/gpl.html
[9]: https://codecov.io/gh/DrylandEcology/rSW2data/branch/master/graph/badge.svg
[10]: https://codecov.io/gh/DrylandEcology/rSW2data
[11]: https://img.shields.io/github/downloads/DrylandEcology/rSW2data/total.svg
[SOILWAT2]: https://github.com/DrylandEcology/SOILWAT2
[STEPWAT2]: https://github.com/DrylandEcology/STEPWAT2
[rSFSTEP2]: https://github.com/DrylandEcology/rSFSTEP2
[rSW2utils]: https://github.com/DrylandEcology/rSW2utils
[rSFSTEP2]: https://github.com/DrylandEcology/rSFSTEP2
[rSOILWAT2]: https://github.com/DrylandEcology/rSOILWAT2
[rSW2data]: https://github.com/DrylandEcology/rSW2data
[rSFSW2]: https://github.com/DrylandEcology/rSFSW2
[issues]: https://github.com/DrylandEcology/rSW2data/issues
[pull request]: https://github.com/DrylandEcology/rSW2data/pulls
[guidelines]: https://github.com/DrylandEcology/workflow_guidelines
[semantic versioning]: https://semver.org/
[testthat]: https://github.com/hadley/testthat
[roxygen2 formatting]: https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html
[r-pkgs man]: http://r-pkgs.had.co.nz/man.html
[r-pkgs tests]: http://r-pkgs.had.co.nz/tests.html


<br>

# rSW2data

Collection of functions to access external data sources and to
provide input data for [rSOILWAT2][], [rSFSW2][], and [rSFSTEP2][]
simulation experiments.


<br>

Please cite the package if you publish results based on code carried
out with our package, see `citation("rSW2data")`, and we would like to hear
about your publication.

<br>


## Table of contents

1. [How to get started](#get_started)
    1. [Installation](#install)
    2. [Documentation](#get_documentation)
2. [How to contribute](#contribute)
    1. [Code guidelines](#follow_guidelines)
    2. [Code documentation](#code_documentation)
    3. [Code tests](#code_tests)
    4. [Code versioning](#code_versioning)
3. [Additional notes](#more_notes)

<br>

<a name="get_started"></a>
## How to get started

<a name="install"></a>
## Installation

```{r}
remotes::install_github("DrylandEcology/rSW2data")
```

<br>

<a name="get_documentation"></a>
### Documentation
View package documentation in an interactive R session with
`help(package = "rSW2data")`


<br>

<a name="contribute"></a>
## How to contribute
You can help us in different ways:

1. Reporting [issues][]
2. Contributing code and sending a [pull request][]

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this
project you agree to abide by its terms.

<br>


<a name="follow_guidelines"></a>
### Follow our guidelines as detailed [here][guidelines]

<br>


### Tests, documentation, and code

<a name="code_documentation"></a>
#### Code documentation
  * This is based on the section
    ['Object documentation' of the book 'R packages' by Wickham][r-pkgs man]
  * Use [roxygen2 formatting][] to write inline code documentation of functions
  * Use regular R-style comments to additionally document code
  * Update help pages and the `NAMESPACE` file with the command
    `devtools::document()`
  * Add examples to function documentation and check that these examples work
    with the command `devtools::run_examples()`

<br>

<a name="code_tests"></a>
#### Code tests and package checks
  * This is based on the section
    ['Testing' of the book 'R packages' by Wickham][r-pkgs tests]

  * Unit tests
    * Use [testthat][] to add unit tests to the existing framework
    * Run unit tests with the command `devtools::test()`

  * Package checks
    * Package checks are run with
      `devtools::check(cran = TRUE, env_vars = c(NOT_CRAN = "true"))` or
      `R CMD build . && NOT_CRAN = "true" R CMD check *.tar.gz`
    * Package checks include unit tests, code style, and spelling
    * Checks will be run on the continuous integration frameworks
      'travis' and 'appveyor' when commits are pushed
    * Development/feature branches can only be merged into master if they pass
      all checks

<br>

<a name="code_versioning"></a>
#### Version numbers

We attempt to follow guidelines of [semantic versioning][] with version
numbers of `MAJOR.MINOR.PATCH`.

If the version numbers changes, then the following files must be updated
* `DESCRIPTION`: adjust lines 'Version'


<br>

<a name="more_notes"></a>
## Notes

### Funding
Work on this package has been supported by various funds managed by
Dr. John Bradford (USGS), Dr. Bill Lauenroth (Yale University),
Dr. Kyle Palmquist (Marshall University), and Dr. Daniel Schlaepfer.


<br>

### License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, [version 3 of the License](LICENSE.md).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


<br>
