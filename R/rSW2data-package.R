################################################################################
# rSW2data: Input data for SOILWAT2 and STEPWAT2 simulation experiments.
# Copyright (C) 2019 Daniel Schlaepfer, John Bradford, William Lauenroth,
#   Kyle Palmquist
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################



#' Package \pkg{rSW2data}: summary information
#'
#' @docType package
#' @name rSW2data
"_PACKAGE"


##------ Package level variables
rSW2_glovars <- new.env()


##------ Import from other packages
#' @import methods
#' @importFrom stats aggregate coef complete.cases cor cov var fitted median
#'   na.exclude na.omit predict quantile sd weighted.mean
NULL
