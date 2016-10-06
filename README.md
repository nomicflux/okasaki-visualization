# Visualization of Okasaki's Purely Functional Data Structures

Demo found here: [http://52.88.174.202:8080](http://52.88.174.202:8080)

The goal of this project is to provide interactive visualizations for the data
strutures in Okasaki's Purely Functional Data Structures, plus other interesting
functional structures that may come up.

Currently, the focus is to get a few simple examples running, namely for Stacks
(i.e. Linked Lists) and Queues.  Once they can be visualized, the
next step is to bring up code snippets for each function, ideally in multiple
functional languages.  Then I'll abstract out the framework and work through
Okasaki in force.

The project is written in Elm for the time being, but I'll probably move over to
Purescript soon - I'm relying on being able to store objects with values, unique
ids, and metadata within the data structures, and Elm doesn't allow me to define
my own `comparable` instances for the data structures which require some sort of
`Ord` constraint.
