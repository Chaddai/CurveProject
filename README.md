CurveProject
============

Curve Plotting Web GUI for the math teacher (that uses LaTeX and Tikz or PSTricks). Realized with Haskell tools.

Features
--------

* Describe a curve by some of its points and optionally the slope of the tangent in those points.
* Configure axis and grid aspect.
* Decide in which points (specified in the GUI) of the curve you want a tangent drawn and configure the tangents aspect.
* Immediate visual feedback thanks to a SVG output (using diagrams-svg) and Tikz and PSTricks outputs for easy and seamless integration in a LaTeX document.
* Autosave and restore session whatever the user do

TODO
----

1) provide named sessions to allow for multiple curve projects (GUI modification)
2) materialize the styling options for curves in the GUI (GUI modification)
3) provide a tabbed interface to specify several curves simultaneously (GUI modification)
4) improve the colour support with a simple parser for the xcolour notation (blue!30!white for a 30% blue 70% white mix) that translate it for SVG (easy with Data.Colour) and PSTricks (not so easy) (engine modification)
5) allows the user to specify different types of curves (especially plotting functions, maybe Hermite interpolation to add to the current ad-hoc solution). (Hard to do well, Engine then GUI modification)
6) ???
7) profit (well at least for my classes)


