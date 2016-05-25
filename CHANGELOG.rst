=========
CHANGELOG
=========

All notable changes to this project will be documented in this file.
This project adheres to `Semantic Versioning <http://semver.org/>`_.


**********
Unreleased
**********


*******************
v0.3.1 - 2016-05-24
*******************

Fixed
=====

* Prevent the output of malformed ASCII terminal control characters
  during the first update.


*******************
v0.3.0 - 2016-05-23
*******************

Added
=====

* Version CLI option.

Changed
=======

* Switch to using a library
  for parsing CLI options.


*******************
v0.2.1 - 2016-05-19
*******************

Fixed
=====

* Moving the cursor up
  to replace the previous status update
  now works on Unix.


*******************
v0.2.0 - 2016-05-18
*******************

Added
=====

* Option to display percentage for each line.
* Throttle output by updating once per second.


*******************
v0.1.0 - 2015-11-13
*******************

Added
=====

* Minimum viable product (MVP) functionality.
  Updates the screen with running counts.
* Imitates the output format of ``sort | uniq --count``
  by sorting output alphabetically,
  displaying the count first,
  and right justifying the count by padding with up to three spaces.
