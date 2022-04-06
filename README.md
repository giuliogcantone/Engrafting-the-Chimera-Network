README
================
Giulio G. Cantone
5/4/2022

This is a guide for the generating a network from a mixture of
multy-layers in R.

# Why I need this in my life?

Because…

## Packages that needed to make the grafting to work

In this guide I will install some useful packages through a package
called `pacman`. If you already have these packages installed, you can
ignore this section.

### Tidyverse is my basic syntax for data wrangling.

``` r
pacman::p_install("tidyverse")
```

### Tidygraph is a syntax for networks manipulation that is inspired by tidyverse. It is built on igraph, and some operations could still be performed on igraph.

``` r
pacman::p_install("igraph")
```

``` r
pacman::p_install("tidygraph")
```

# How to generate deterministic or stochastics graphs with igraph and tidygraph

If you feel confident in this stuff, just jump to the next section.

### Deterministic graph are exact graphs.

A deterministic graph is a graph that has an exact connecting propriety
applied over a number of nodes. For example a “complete graph” or
“clique” has the propriety that each node of the graph is connected to
each other.

In `igraph`, to construct a complete graph, you write this code:

``` r
igraph::make_full_graph(10, directed = FALSE) -> g1
g1
```

    ## IGRAPH 52d69e0 U--- 10 45 -- Full graph
    ## + attr: name (g/c), loops (g/l)
    ## + edges from 52d69e0:
    ##  [1] 1-- 2 1-- 3 1-- 4 1-- 5 1-- 6 1-- 7 1-- 8 1-- 9 1--10 2-- 3 2-- 4 2-- 5
    ## [13] 2-- 6 2-- 7 2-- 8 2-- 9 2--10 3-- 4 3-- 5 3-- 6 3-- 7 3-- 8 3-- 9 3--10
    ## [25] 4-- 5 4-- 6 4-- 7 4-- 8 4-- 9 4--10 5-- 6 5-- 7 5-- 8 5-- 9 5--10 6-- 7
    ## [37] 6-- 8 6-- 9 6--10 7-- 8 7-- 9 7--10 8-- 9 8--10 9--10

``` r
g1 %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> \#\#
Stochastic graphs have a random distributions of edges, following a
model.

So, if you randomly generate them, you need to set a “seed” to see
always the same outcome. If you don’t, there will be a different output
each time.

``` r
set.seed(999)
igraph::random.graph.game(10, p.or.m = .2, directed = FALSE) -> g2
g2
```

    ## IGRAPH 52e1adb U--- 10 10 -- Erdos renyi (gnp) graph
    ## + attr: name (g/c), type (g/c), loops (g/l), p (g/n)
    ## + edges from 52e1adb:
    ##  [1] 3-- 4 5-- 7 1-- 9 2-- 9 3-- 9 5-- 9 8-- 9 2--10 7--10 9--10

``` r
g2 %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# If you, like me, don’t like the igraph format, tidygraph will help you

Deterministic and random graph constructor are easy to recognize in
`tidygraph` because deterministic graph generation always starts with
the verb `create_`:

``` r
tidygraph::create_complete(10) -> g1
g1
```

    ## # A tbl_graph: 10 nodes and 45 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 x 0 (active)
    ## # ... with 4 more rows
    ## #
    ## # Edge Data: 45 x 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     1     3
    ## 3     1     4
    ## # ... with 42 more rows

``` r
g1
```

    ## # A tbl_graph: 10 nodes and 45 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 x 0 (active)
    ## # ... with 4 more rows
    ## #
    ## # Edge Data: 45 x 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     1     3
    ## 3     1     4
    ## # ... with 42 more rows

while the random graphs are generated throug the verb `play_`:

``` r
set.seed(999)
tidygraph::play_erdos_renyi(10, p = .2, directed = FALSE) -> g2
g2
```

    ## # A tbl_graph: 10 nodes and 10 edges
    ## #
    ## # An undirected simple graph with 2 components
    ## #
    ## # Node Data: 10 x 0 (active)
    ## # ... with 4 more rows
    ## #
    ## # Edge Data: 10 x 2
    ##    from    to
    ##   <int> <int>
    ## 1     3     4
    ## 2     5     7
    ## 3     1     9
    ## # ... with 7 more rows

You can notice that now the format look like `tibble` from `tidyverse`.
This is not a coincidence. This format allows to visualize the
attributes of the networks as two relational tables.

Have you noticed that `g1` and `g2` have no columns under the table of
the nodes?

One could be mislead to think that this means that those nodes does not
exist! But they exists! They are the “invisible” row of the Node Data
tibble. If you give a second sight you will see “10 x 0 (active)”. This
means that there are 10 nodes in the graph.

However, the tibbles of the edges has already 2 attributes: `from` and
`to`. These are very particular variables, because usually they cannot
be `mutated`. We will learn how to correct manipulate these to engraft
our Chimera Network! `from` and `to` also always point to the `rowID` of
the nodes, and if `rowID` are re-shuffled, values in `from` and `to`
will change to.

## Mutate within tidygraph!

Don’t you think that those nodes are a bit hard to recognize?

Tidygraph makes very easy to manipulate attributes in networks:

``` r
set.seed(999)
g1 %>%
  mutate(
    id =
      cur_group_rows(),
    color = sample(
    c("red", "blue"),
    n(),
    replace = T)
    ) -> g1
g1
```

    ## # A tbl_graph: 10 nodes and 45 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 x 2 (active)
    ##      id color
    ##   <int> <chr>
    ## 1     1 red  
    ## 2     2 blue 
    ## 3     3 red  
    ## 4     4 red  
    ## 5     5 red  
    ## 6     6 blue 
    ## # ... with 4 more rows
    ## #
    ## # Edge Data: 45 x 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     1     3
    ## 3     1     4
    ## # ... with 42 more rows

``` r
g1 %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Here 2 notes are necessary:

-   We gave an `id` to each node as the CURRENT row. This `id` is not
    related stably to values in `from` and `to`, because these always
    look for the current `row` of the row, not for a static `id`
    attribute.
-   `plot()` - as ugly as it is - is a very smart guy and it recognize
    the attribute `color`!

### How can we assign attributes to the links?

Remember the weird wording `(active)`? For each `tidygraph` there is
always an active part: the nodes or the edges.

For assigning an attribute to the edges, we need to `activate` the
`edges`.

``` r
set.seed(999)
g1 %>% activate(edges) %>%
  mutate(color = sample(
    c("orange", "aquamarine"),
    n(),
    replace = T,
    p=c(.5,.5)
    )) -> g1
g1
```

    ## # A tbl_graph: 10 nodes and 45 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Edge Data: 45 x 3 (active)
    ##    from    to color     
    ##   <int> <int> <chr>     
    ## 1     1     2 aquamarine
    ## 2     1     3 orange    
    ## 3     1     4 aquamarine
    ## 4     1     5 orange    
    ## 5     1     6 orange    
    ## 6     1     7 aquamarine
    ## # ... with 39 more rows
    ## #
    ## # Node Data: 10 x 2
    ##      id color
    ##   <int> <chr>
    ## 1     1 red  
    ## 2     2 blue 
    ## 3     3 red  
    ## # ... with 7 more rows

``` r
g1 %>% plot()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

# The essence of Network Grafting:
