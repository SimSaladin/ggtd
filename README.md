# ggtd


## Installation

     git clone ...
     cd ggtd
     stack build
     stack exec ggtd

## Usage

New task

    node "Do the laundry"

New aggregated context (link)

    node -rlink "@home"

Add stuff to an aggregated context

    rel laundry @home

(in the above example, laundry and @home are resolved automatically to correct
nodes.)

Add a group of items into an aggregated context as a group:
just set the correct rel.

    rel -rgroup chores @home

Remove all relation from a node to a node:

    rel delete @home chores

## Tickler syntax

    tickler add "somenode" monthly done
    tickler add "somenode" nextmonth wait
    tickler add "somenode" daily:ke wait

The last argument indicates which flag to unset.

## Nice-to-have-things

- Integration possibilities with things like github issues
- More library-oriented design; user could build a customized instance of the
    program on top of the library.
- Toggleable filtering for done etc. in listing
- Fuzzy matching of nodes - unique substring match in nodes under view context?
- Timed nodes: specify remind-like flag for triggers which add an edge to the
    node from a tracking node
- Sorting by relevance
- Default connect new childs to some node that is used to track incoming things
