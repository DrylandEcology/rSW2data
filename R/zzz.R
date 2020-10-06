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




.onLoad <- function(libname, pkgname) {
  #--- Define package level variables that should be hidden from package user
  # 'rSW2_glovars' is defined in rSW2data-package.R

  assign("tol", sqrt(.Machine[["double.eps"]]), envir = rSW2_glovars)
  assign("st_mo", seq_len(12L), envir = rSW2_glovars)

  invisible()
}
