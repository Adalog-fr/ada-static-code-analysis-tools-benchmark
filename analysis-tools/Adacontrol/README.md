# AdaControl

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/adactl.json)](https://alire.ada.dev/crates/adactl.html)

AdaControl is an Ada rules controller. It is used to control that Ada software meets the requirements of a number of parameterizable rules. It is not intended to supplement checks made by the compiler, but rather to search for particular violations of good-practice rules, or to check that some rules are obeyed project-wide.

Please refer to the [User's guide](https://www.adalog.fr/compo/adacontrol_ug.html) for information about how to install and run AdaControl. If you are an Alire user, you only have to take care of ASIS (see below). If you used the automatic setup under Windows, everything is installed and the guide is accessible from the "Help/Adacontrol/User guide" menu from GPS.

Otherwise you'll find the guides in a variety of formats (adacontrol_ug.html, adacontrol_ug.pdf or adacontrol_ug.info) in the "doc" directory of the installation.

## ASIS Dependency

**Important**: you need to have ASIS in your path (`GPR_PROJECT_PATH` or available in project search path of GNAT) in order to compile.

ASIS is deliberately not put in dependence of this project in order to allow the use of custom builds of ASIS for GNAT Pro customers.

If you do not have a custom build of ASIS, you can download the latest available on AdaCore 2020 (https://www.adacore.com/download/more).

## Commercial Support

Adalog provides commercial support for AdaControl. Support includes
the following services:

* Help with installation procedures.

* Explanations regarding the use of the tool, and help for translating
  coding standards into AdaControl rules.

* Priority handling of tickets reported through our BT system.

* Correction of problems encountered in the use of AdaControl.
  Pre-releases versions of AdaControl are provided for each corrected
  problem.

* Access to beta-versions before they are released.

* Keeping in sync customer's own custom rules with the latest version
  of AdaControl.

* Reduced rate for on-demand development of custom rules.

* Priority consideration of enhancement requests.
  Satisfying enhancement requests is not part of the support
  contract; however, Adalog is constantly improving AdaControl, and
  suggestions originating from supported customers are given a high
  priority in our todo list.

Adalog cannot correct problems whose origin is due to compiler bugs or
defects in the implementation of ASIS (contact your compiler provider
for support on these problems). However, Adalog will do its best
effort to help reporting such problems to the compiler vendor, and to
find workarounds until the problem is fixed.


In addition, Adalog can provide various services:

- Custom improvements to AdaControl, including application-specific
  or company-specific rules;
 
- consulting services for defining coding standards;

- consulting services in all areas related to Ada, real-time,
  compilation, etc. See http://www.adalog.fr for details.

For pricing information about these services, please contact
info@adalog.fr
