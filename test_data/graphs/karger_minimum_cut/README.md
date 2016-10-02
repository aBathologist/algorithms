
# Read Me: Karger Minimum Cut

## What's included:

Included in this folder are sample text files containing graphs.

Each folders contain graphs of different minimum cuts.

- `one_cut_graph` contains graphs were the minimum cut is one.
- `two_cut_graph` contains graphs were the minimum cut is two.
- `three_cut_graph` contains graphs were the minimum cut is three.
- `four_cut_graph` contains graphs were the minimum cut is four.

## File format:

The file format of each file:

- Each file is a complete graph
- Each line describes a vertex and its connections
- The leading integer on the line is the vertex
- Each integer after the initial integer are the connections to other vertices

For example:

````
1 2 3
2 1
3 1
````

for the first line:
  - `1` is the vertex
  - `2` is the vertex `1` is connected to
  - `3` is another vertex `1` is connected to

So, this would describe:

````
   1
  / \
 2   3
````

For more details, reach out to us on Twitter @nlangnyc or join our
slack:

http://nlang-slack-inviter.herokuapp.com/

and join the `#algorithms` channel.

Have fun!

Note: these graphs are test cases from the coursera discussion
forums. If you have more to contribute, please contact us on slack or twitter.