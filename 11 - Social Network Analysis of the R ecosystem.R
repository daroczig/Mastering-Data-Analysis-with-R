## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #11: Social Network Analysis
##                 of the R Ecosystem. pp. 269-280.
##
##
##  This file includes the code chunks from the above mentioned
##  chapter except for the leading ">" and "+" characters, which
##  stand for the prompt in the R console. The prompt was
##  intentionally removed here along with arbitrary line-breaks,
##  so that you copy and paste the R expressions to the R console
##  in a more convenient and seamless way.
##
##  Code chunks are grouped here by the printed pages of the book.
##  Two hash signs at the beginning of a line stands for a page
##  break, while an extra empty line between the code chunks
##  represents one or more paragraphs in the original book between
##  the examples for easier navigation.
##
##  Sometimes extra instructions starting with a double hash are
##  also provided on how to run the below expressions.
##
##
##  Find more information on the book at http://bit.ly/mastering-R
##  and you can contact me on Twitter and GitHub by the @daroczig
##  handle, or mail me at daroczig@rapporter.net
##

library(tools)
pkgs <- available.packages()
str(pkgs)
head(package.dependencies(pkgs), 2)

##

library(plyr)
edges <- ldply(c('Depends', 'Imports', 'Suggests'), function(depLevel) {
    deps <- package.dependencies(pkgs, depLevel = depLevel)
    ldply(names(deps), function(pkg)
        if (!identical(deps[[pkg]], NA))
            data.frame(
                src   = pkg,
                dep   = deps[[pkg]][, 1],
                label = depLevel,
                stringsAsFactors = FALSE))
})
str(edges)

nrow(edges) / (nrow(pkgs) * (nrow(pkgs) - 1))

##

head(sort(table(edges$dep), decreasing = TRUE))

edges <- edges[edges$dep != 'R', ]


## directed


## table(edges$label)
## edges <- subset(edges, label %in% c('Depends', 'Imports'))

library(igraph)
g <- graph.data.frame(edges)
summary(g)

graph.density(g)
head(sort(degree(g), decreasing = TRUE))

##

head(sort(degree(g), decreasing = TRUE))
head(sort(closeness(g)))
head(sort(betweenness(g), decreasing = TRUE))

plot(degree(g), betweenness(g), type = 'n', main = 'Centrality of R package dependencies')
text(degree(g), betweenness(g), labels = V(g)$name)

##

edges <- edges[edges$label != 'Suggests', ]
deptree <- edges$dep[edges$src == 'igraph']
while (!all(edges$dep[edges$src %in% deptree] %in% deptree))
    deptree <- union(deptree, edges$dep[edges$src %in% deptree])

g <- graph.data.frame(edges[edges$src %in% c('igraph', deptree), ])
plot(g)

##

V(g)$label.color <- 'orange'
V(g)$label.color[V(g)$name == 'igraph'] <- 'darkred'
V(g)$label.color[V(g)$name %in% edges$dep[edges$src == 'igraph']] <- 'orangered'
E(g)$color <- c('blue', 'green')[factor(df$label)]
plot(g, vertex.shape = 'none', edge.label = NA)

##

tkplot(g, edge.label = NA)
rglplot(g)

##

library(visNetwork)
nodes <- get.data.frame(g, 'vertices')
names(nodes) <- c('id', 'color')

##

edges <- get.data.frame(g)
visNetwork(nodes, edges)

g <- dominator.tree(g, root = 'igraph')$domtree
plot(g, layout = layout.reingold.tilford(g, root = 'igraph'), vertex.shape = 'none')

##

library(miniCRAN)
pkgs <- pkgAvail()
pkgDep('igraph', availPkgs = pkgs, suggests = FALSE, includeBasePkgs = TRUE)
plot(makeDepGraph('igraph', pkgs, suggests = FALSE, includeBasePkgs = TRUE))
