# Visualization of Okasaki's Purely Functional Data Structures

Demo found here: [https://nomicflux.github.io/okasaki-visualization/](https://nomicflux.github.io/okasaki-visualization/)

The goal of this project is to provide interactive visualizations for the data
strutures in Okasaki's Purely Functional Data Structures, plus other interesting
functional structures that may come up.

Currently, the focus is to get a few simple examples running, namely for Stacks
(i.e. Linked Lists) and Queues.  Once they can be visualized, the
next step is to bring up code snippets for each function, ideally in multiple
functional languages.  Then I'll abstract out the framework and work through
Okasaki in force.

Update: Code snippets are in place for Stacks in multiple languages, and for Queues in Purescript.  They are better for some languages than
others - since the site is written in Purescript, it is of course the main
exemplar, with Elm and Haskell close behind.  Scala is proving difficult to
incorporate, since the structure of the code is more object-oriented and so less
given to nice snippets.  Dynamic languages such as Clojure and Elixir fall in
the middle.
