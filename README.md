# Tailspin

Tailspin is an interactive notebook-style REPL in the browser. It combines an interface inspired by [Mathematica](http://www.wolfram.com/mathematica/) and the [IPython notebook](http://ipython.org/notebook.html) with a "timeless" declarative programming model inspired by spreadsheets and, to a certain extent, [Elm](http://elm-lang.org/). It's sort of trying to become an authoring tool for [explorable explanations](http://worrydream.com/ExplorableExplanations/).

A Tailspin document, or *sheet*, contains *code cells* that describe its interactive behavior. Code is written in a strict subset of the [Clojure](http://clojure.org/) programming language that eschews impure functions and any notion of mutable state. Behind the scenes, data flows from cell to cell by means of a dependency graph. When the code in a cell is updated, the resulting change in that cell's value is automatically propagated downstream to any dependent cells.

Direct manipulation is one of Tailspin's defining features. A code cell's value may depend upon user input (taken from a variety of familiar UI elements) in addition to the code the cell contains. Through such *input cells*, the user may manipulate various properties of the underlying reactive system without having to edit any code.

## Status

Very WIP. Lots of conceptual stuff is still in flux and the current UI is more a proof of concept than anything else.
