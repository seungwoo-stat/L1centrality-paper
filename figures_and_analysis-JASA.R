### This file is structured as follows:
### 
### 1. Analysis of the MCU movie network
### 2. Analysis of the assembly network
### 3. Others
### 

### [Note] L1centLOC() is a right-continuous step function in `alpha`. Hence, we
### added a small number to our intended `alpha` to prevent rounding errors,
### which would not affect the intended result. E.g., `alpha = 3.1/n` instead of
### `alpha = 3/n` (n is the number of vertices).

library(igraph)
library(latex2exp)
library(L1centrality)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

################################################################################
## MCU movie network ###########################################################
################################################################################

## Fig 2: L1 centrality of MCU movie network
## save as 4 x 12 plot
df <- data.frame(y=rep(0,32),x=L1cent(MCUmovie, eta = V(MCUmovie)$worldwidegross))
rownames(df) <- V(MCUmovie)$name # data.frame of L1 centrality values (column `x`)
ggplot(df, aes(x, y, label=rownames(df))) +
  geom_point(size=2) +
  geom_text_repel(nudge_y = 0.2,  direction = 'x', angle = 45,
                  vjust = "inward", segment.size = 0.2) +
  ylim(-0.1, 0.4) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  # xlim(0.00-0.3, 1.0) +
  theme_classic() +
  coord_cartesian(clip = 'off') +
  labs(x=TeX("$L_1$ centrality"), y="") +
  theme(axis.line.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))


## Fig 3: MCU movie network's Fruchterman-Reingold plot, Target plot
## save as 6 x 12 plot
set.seed(0)
params <- L1centMDS(MCUmovie)
par(mfrow=c(1,2), mar = c(5, 4, 4, 2) + 0.1)
# Fig 3(a)
plot(MCUmovie, layout = layout_with_fr, vertex.size = c(rep(5,18),7,rep(5,13)),
     vertex.color=c(rep("gray",18),"black",rep("gray",13)),
     vertex.label.cex = 0.6, vertex.label.dist = 0, vertex.label.color="black",
     main="(a) Fruchterman-Reingold layout plot", vertex.label.family = "sans")
# Fig 3(b)
plot(params, main="(b) Target plot", plot.col="black",
     plot.cex=1.5, plot.pch=21, plot.bg=c(rep("gray",18),"black",rep("gray",13)))


## Fig 6: L1 centrality of MCU movie network symmetrized w.r.t. `Spider-Man: No
## Way Home`
## save as 4 x 12 plot
df <- data.frame(y=rep(0,32),
                 x=L1centNB(MCUmovie, eta = V(MCUmovie)$worldwidegross)$`Spider-Man: No Way Home`)
rownames(df) <- V(MCUmovie)$name
# data.frame of L1 centrality values in the symmetrized graph (column `x`)
ggplot(df, aes(x, y, label=rownames(df))) +
  geom_point(size=2) +
  geom_text_repel(nudge_y = 0.2,  direction = 'x', angle = 45,
                  vjust = "inward", segment.size = 0.2) +
  ylim(-0.1, 0.4) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  # xlim(0.00-0.3, 1.0) +
  theme_classic() +
  coord_cartesian(clip = 'off') +
  labs(x=TeX("$L_1$ centrality"), y="") +
  theme(axis.line.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))


## Fig 1 (supplement): Comparing L1 centrality - unweighted vs. weighted vertex
## save as 4 x 9 plot
mcu0 <- L1cent(MCUmovie)
mcu1 <- L1cent(MCUmovie, eta = V(MCUmovie)$worldwidegross)

layout(matrix(c(1,2,3), nrow=1), width = c(2,2,1.2))

# Fig 1 (supplement) - left panel
par(mar=c(5,4,4,2) + 0.1)
plot(NA, xlim = c(0,1), ylim = c(0,1), xaxt="n", bty = "l",
     xlab = "Multiplicity", ylab = TeX("$L_1$ centrality"))
axis(1, c(0,1), c("Equal","Worldwide gross"), cex.axis = 1)
for(i in 1:32) lines(c(0,1),c((mcu0)[i], (mcu1)[i]))

# Fig 1 (supplement) - right panel
par(mar=c(5,4,4,-0.1) + 0.1)
large.change.index <- abs(rank(mcu0) - rank(mcu1)) >= 3
plot(NA, xlim = c(0,1), ylim = c(0,1), xaxt="n", bty = "l",
     xlab = "Multiplicity", ylab = TeX("$L_1$ centrality (uniform margin)"))
axis(1, c(0,1), c("Equal","Worldwide gross"), cex.axis = 1)
for(i in 1:32){
  lines(c(0,1),c(ecdf(mcu0)(mcu0)[i], ecdf(mcu1)(mcu1)[i]), 
        col = ifelse(large.change.index[i], "black", "gray"))
}

par(mar=c(5,-0.1,4,2) + 0.1)
plot(NA, xlim = c(0,1.3), ylim = c(0,1), xaxt="n", yaxt="n", bty = "n",
     xlab = "", ylab = "")
for(i in 1:32){
  if(large.change.index[i]){
    text(0, ecdf(mcu1)(mcu1)[i], names(mcu1)[i], pos=4)
  }
}
par(mfrow = c(1,1), mar=c(5,4,4,2) + 0.1)


## Fig 5 (supplement): Comparing L1 centrality to other measures
## save as 10 x 12 plot
l1 <- L1cent(MCUmovie)
MCUmovie.no.wt <- MCUmovie
E(MCUmovie.no.wt)$weight <- rep(1,278)
l1.no.wt <- L1cent(MCUmovie.no.wt)
deg <- degree(MCUmovie)
bet <- betweenness(MCUmovie)
clo <- closeness(MCUmovie)
eig <- eigen_centrality(MCUmovie, weights = NA)$vector
# pdf(file = "mcu-comparison.pdf", width = 10, height = 12)
par(mfcol=c(4,2))
plot(l1, deg, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Degree centrality")
plot(l1, clo, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Closeness centrality")
plot(l1, bet, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Betweeness centrality")
plot(l1, eig, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Eigenvector centrality")
plot(ecdf(l1)(l1), ecdf(deg)(deg), pch=20, 
     xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Degree centrality (uniform margin)")
plot(ecdf(l1)(l1), ecdf(clo)(clo), pch=20, 
     xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Closeness centrality (uniform margin)")
plot(ecdf(l1)(l1), ecdf(bet)(bet), pch=20, 
     xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Betweeness centrality (uniform margin)")
plot(ecdf(l1)(l1), ecdf(eig)(eig), pch=20, 
     xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Eigenvector centrality (uniform margin)")
# dev.off()
# correlation between the measures
cor(l1, deg); cor(l1, clo); cor(l1, bet); cor(l1, eig)
cor(l1, deg, method="spearman"); cor(l1, clo, method="spearman"); 
cor(l1, bet, method="spearman"); cor(l1, eig, method="spearman")


## Table 1 (supplement): Comparing L1 centrality to other measures
cbind(
  names(sort(-L1cent(MCUmovie))[1:5]),
  names(sort(-degree(MCUmovie))[1:5]),
  names(sort(-closeness(MCUmovie))[1:5]),
  names(sort(-betweenness(MCUmovie))[1:5]),
  names(sort(-eigen_centrality(MCUmovie, weights = NA)$vector)[1:5])
) |> knitr::kable(format = "latex")


## Fig 8 (supplement): Multiscale edge representation of MCU movie network
## save as 4 x 12 plot
MCU.edge <- L1centEDGE(MCUmovie, eta = V(MCUmovie)$worldwidegross, alpha = c(8.1, 16.1, 32)/32) # add 0.1 to prevent rounding errors
MCU.edge[[1]] |> graph_from_edgelist(directed=TRUE) -> g8
MCU.edge[[2]] |> graph_from_edgelist(directed=TRUE) -> g16
MCU.edge[[3]] |> graph_from_edgelist(directed=TRUE) -> g32
mculabel8 <- V(g8)$name
mculabel8[!mculabel8 %in% c("Avengers: Endgame","Avengers: Infinity War")] <- NA
mculabel16 <- V(g16)$name
mculabel16[!mculabel16 %in% c("Avengers: Endgame","Avengers: Infinity War",
                              "Ant-Man and the Wasp","Ant-Man and the Wasp: Quantumania")] <- NA
mculabel32 <- V(g32)$name
mculabel32[!mculabel32 %in% c("Avengers: Infinity War")] <- NA

par(mfrow=c(1,3))
set.seed(0)
# Fig 8(a) (supplement)
plot(g8,vertex.color="gray",vertex.label.cex = 1,vertex.label.dist = 0,
     vertex.label.color="black",
     vertex.size=sapply(V(g8)$name,\(n)ifelse(n %in% c("Avengers: Infinity War", "Avengers: Endgame"), 15, 7)),
     vertex.label.family = "sans",edge.arrow.size=0.5,
     main=TeX("(a) $\\alpha$=8/32",bold=TRUE),vertex.label=mculabel8)
# Fig 8(b) (supplement)
plot(g16,vertex.color="gray",vertex.label.cex = 1,vertex.label.dist = 0,
     vertex.label.color="black",
     vertex.size=sapply(V(g16)$name,\(n)ifelse(n %in% c("Avengers: Infinity War", "Avengers: Endgame"), 15, 7)),
     vertex.label.family = "sans",edge.arrow.size=0.5,
     main=TeX("(b) $\\alpha$=16/32",bold=TRUE),vertex.label=mculabel16)
# Fig 8(c) (supplement)
plot(g32,vertex.color="gray",vertex.label.cex = 1,vertex.label.dist = 0,
     vertex.label.color="black",
     vertex.size=sapply(V(g32)$name,\(n)ifelse(n %in% c("Avengers: Infinity War"), 15, 7)),
     vertex.label.family = "sans",edge.arrow.size=0.5,
     main=TeX("(c) $\\alpha$=32/32",bold=TRUE),vertex.label=mculabel32)


## Fig 10 (supplement): Lorenz curve of MCU movie network
## save as 8 x 8 plot
par(mfrow=c(1,1))
L1cent(MCUmovie) |> Lorenz_plot(asp=1, main="Lorenz plot of MCU movie network")
L1cent(MCUmovie, eta = V(MCUmovie)$worldwidegross) |> Lorenz_plot(add=TRUE, lty=2)
L1cent(MCUmovie, eta = 1/V(MCUmovie)$worldwidegross) |> Lorenz_plot(add=TRUE, lty=4)
legend("topleft", lty=c(0,1,2,4), legend=c(as.expression(bquote(bold("Multiplicity"))),
                                           "Equal","Worldwide gross","1/(Worldwide gross)"),bty="n")
c(Gini(L1cent(MCUmovie)), 
  Gini(L1cent(MCUmovie, eta = V(MCUmovie)$worldwidegross)), 
  Gini(L1cent(MCUmovie, eta = 1/V(MCUmovie)$worldwidegross))) |> 
  round(4) # heterogeneity indices for three graphs
cor.test(V(MCUmovie)$worldwidegross, L1cent(MCUmovie), alternative = "greater")

################################################################################
## assembly network ############################################################
################################################################################

color.party <- c("black","#004EA1","black","#FFED00","#E61E2B","black","black")
data(rokassembly21)
# D2, D181: Two chairmen of the 21st National Assembly -> delete them from the graph
rok.reduce <- delete_vertices(rokassembly21, c(V(rokassembly21)$name[!V(rokassembly21)$full], "D2", "D181"))

## Fig 7: assembly network - Target plot + global vs. local (alpha = 15/279)
## save as 8 x 17 plot
rok.global <- L1cent(rok.reduce)
rok.local <- L1centLOC(rok.reduce, alpha = 15.1/279) # 15.1 to prevent rounding errors

opar <- par(oma=c(0,0,0,9.5),mar=c(5,4,4,1)+0.1,mfrow=c(1,2))
set.seed(0)
params.rok.full <- L1centMDS(rokassembly21)
# Fig 7(a)
plot(params.rok.full, plot.bg=color.party[V(rokassembly21)$party],
     plot.pch=sapply(V(rokassembly21)$full,\(f)ifelse(f,21,4)),
     text.labels="", main="(a) Target plot of 317 members",
     plot.col=sapply(1:317,\(n)ifelse(V(rokassembly21)$full[n],"black",color.party[V(rokassembly21)$party[n]])))
# Fig 7(b)
outlier.index <- which(abs(ecdf(rok.global)(rok.global) - ecdf(rok.local[[1]])(rok.local[[1]])) >= 0.5)
plot(ecdf(rok.global)(rok.global),
     ecdf(rok.local[[1]])(rok.local[[1]]),
     asp = 1,
     pch = 21, col="black", bg = color.party[V(rok.reduce)$party],
     xlab = TeX("$L_1$ centrality (uniform margin)"),
     ylab = TeX("Local $L_1$ centrality, $\\alpha = 15/279$ (uniform margin)"),
     main=TeX("(b) Global vs. local $L_1$ centrality",bold = TRUE)
     )
abline(0.5,1); abline(-0.5,1)
text(ecdf(rok.global)(rok.global)[outlier.index],
     ecdf(rok.local[[1]])(rok.local[[1]])[outlier.index],
     V(rok.reduce)$name[outlier.index], pos=3)
legend("topright",inset=c(-0.40,-0.01),xpd=NA,bty="n",
       pch=rep(21,4),
       legend=c("Democratic Party", "People Power Party", "Justice Party", "Others"),
       pt.bg=color.party[c(2,5,4,1)], col="black")
par(opar)


## Analysis of the Justice Party and Table 1
# number of cosponsored bills between two members of the Justice Party
(1/(as_adjacency_matrix(rok.reduce, attr="weight")[paste0("J",1:6),paste0("J",1:6)])) |> as.matrix() |> knitr::kable(format="latex")

# number of cosponsored bills of each member of the Justice Party (Table 1)
sapply(paste0("J",1:6), \(n) V(rok.reduce)$nbill[V(rok.reduce)$name==n])

# distribution of cosponsored bills of two members in the reduced assembly network
(1/(as_adjacency_matrix(rok.reduce, attr="weight"))) |> as.vector() -> cospon.dist
cospon.dist[cospon.dist == Inf] <- NA
summary(cospon.dist)
ecdf(cospon.dist)(416)

# cosponsored bills of each Justice Party member with the other member in the
# reduced assembly network: top 6.
lapply(paste0("J",1:6),
       \(n){
         temp <- (1/(as_adjacency_matrix(rok.reduce, attr="weight"))[n,]) 
         temp[temp != Inf] |>
         sort(decreasing = TRUE) |>
         head(6)
         })

# distribution of cosponsored bill by each member in the reduced assembly network
summary(V(rok.reduce)$nbill)


## Analysis of the P90, P57 and Figure 6 in the supplement (8 x 8 plot)
# proportion of cosponsored bills between the two parties - to the cosponsored
# bill within each party.
edgeconnection <- data.frame(weight = 1/E(rok.reduce)$weight,
                             V(rok.reduce)$party[get.edges(rok.reduce,es=E(rok.reduce))] |> matrix(ncol=2),
                             V(rok.reduce)$name[get.edges(rok.reduce,es=E(rok.reduce))] |> matrix(ncol=2))
names(edgeconnection) <- c("weight","p1","p2","n1","n2")

between.weight <- edgeconnection$weight[
  (edgeconnection$p1 == "Democratic Party of Korea" & edgeconnection$p2 == "People Power Party") |
    (edgeconnection$p2 == "Democratic Party of Korea" & edgeconnection$p1 == "People Power Party")
]
DP.weight <- edgeconnection$weight[
  (edgeconnection$p1 == "Democratic Party of Korea" & edgeconnection$p2 == "Democratic Party of Korea")
]
PP.weight <- edgeconnection$weight[
  (edgeconnection$p2 == "People Power Party" & edgeconnection$p1 == "People Power Party")
]
summary(between.weight)
summary(DP.weight)
summary(PP.weight)
par(mfrow=c(1,1))
boxplot(DP.weight, PP.weight, between.weight, names=c("DP-DP","PPP-PPP","DP-PPP"),
        main="Number of cosponsored bills of the two major parties")

# connection between the two major parties, top 10, excluding the exceptional
# member P9, who moved from PPP to DP
edgeconnection[
  (edgeconnection$p1 == "Democratic Party of Korea" & edgeconnection$p2 == "People Power Party") |
    (edgeconnection$p2 == "Democratic Party of Korea" & edgeconnection$p1 == "People Power Party"),] -> between.table
between.table <- between.table[between.table$n1 != "P9" & between.table$n2 != "P9",]
between.table[order(between.table$weight,decreasing = TRUE)[1:10],] # cosponsorship between the two major parties; sorted

# quantile of the `bridge` vertices
rok.reduce.l1cent <- L1cent(rok.reduce)
sapply(c("P90","P57","P8","D139","D63","D128","D140"),
       \(n) ecdf(rok.reduce.l1cent)(rok.reduce.l1cent)[V(rok.reduce)$name==n])

# number of cosponsored bills by P90 and P57:
# they are the smallest among the members whose global L1 centrality >= 90% and 95% quantile
V(rok.reduce)$nbill[which(V(rok.reduce)$name == "P90")] #905
V(rok.reduce)$nbill[which(V(rok.reduce)$name == "P57")] #467
V(rok.reduce)$nbill[which(ecdf(rok.reduce.l1cent)(rok.reduce.l1cent) >= 0.9)] |> sort()
V(rok.reduce)$nbill[which(ecdf(rok.reduce.l1cent)(rok.reduce.l1cent) >= 0.95)] |> sort()


## Fig 7 (supplement): Functional data set extracted from the reduced assembly network
## save as 8 x 8 plot
rok.local.all <- L1centLOC(rok.reduce,alpha=pmin((seq(5,275,by=5) + 0.1)/279,1)) # adding 0.1 to prevent rounding errors
rok.local.all.mat <- do.call(rbind,rok.local.all)
rok.local.all.mat.unif <- apply(rok.local.all.mat,1,\(r)ecdf(r)(r)) |> t()
colnames(rok.local.all.mat.unif) <- colnames(rok.local.all.mat)
rok.local.all.mat.unif.normalize <- apply(rok.local.all.mat.unif,2,\(cc)cc-mean(cc))
plot(seq(5,275,by=5)/279,
     rok.local.all.mat.unif.normalize[,1], type="l", ylim=range(rok.local.all.mat.unif.normalize),
     xlab=TeX("\\alpha"),ylab=TeX("Standardized $L_1$ centrality (uniform margin)"),
     main="Functional data from the assembly network")
for(i in 2:279){
  lines(seq(5,275,by=5)/279,
        rok.local.all.mat.unif.normalize[,i], type="l", col=color.party[V(rok.reduce)$party[i]])
}
for(i in which(startsWith(colnames(rok.local.all.mat),"J"))){
  lines(seq(5,275,by=5)/279,
        rok.local.all.mat.unif.normalize[,i], type="l", col=color.party[V(rok.reduce)$party[i]])
}
legend("topright",bty="n",lty=1,lwd=2,
       legend=c("Democratic Party", "People Power Party", "Justice Party", "Others"),
       col=color.party[c(2,5,4,1)])


## Fig 9 (supplement): Multiscale edge representation of the reduced assembly network
## save as 8 x 17 plot
edges <- L1centEDGE(rok.reduce, alpha = c(15.1/279, 279/279)) # set to 15.1 to prevent rounding errors
edges[[1]] |> graph_from_edgelist() -> g15
edges[[2]] |> graph_from_edgelist() -> g279
localmeds15 <- unique(edges[[1]][,2])
localmeds279 <- unique(edges[[2]][,2])

set.seed(0)
par(mfrow=c(1,2))
# Fig 9(a) (supplement)
g15 |> plot(layout = layout_with_kk,
            vertex.color=sapply(V(g15)$name,\(n)color.party[V(rok.reduce)$party[V(rok.reduce)$name==n]]),
            vertex.label.cex = 1,vertex.label.dist = 0,
            vertex.label.color=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds15,"white","black") ),
            vertex.size=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds15,15,3) ),
            vertex.label.family = "sans",edge.arrow.size=0.5,
            vertex.label = sapply(V(g15)$name,\(n)ifelse(n %in% localmeds15, n, NA)),
            main=TeX("(a) $\\alpha = 15/279$", bold=TRUE))
# Fig 9(b) (supplement)
g279 |> plot(layout = layout_with_fr,
             vertex.color=sapply(V(g15)$name,\(n)color.party[V(rok.reduce)$party[V(rok.reduce)$name==n]]),
             vertex.label.cex = 1,vertex.label.dist = 0,
             vertex.label.color=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds279,"white","black") ),
             vertex.size=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds279,15,3) ),
             vertex.label.family = "sans",edge.arrow.size=0.5,
             vertex.label = sapply(V(g15)$name,\(n)ifelse(n %in% localmeds279, n, NA)),
             main=TeX("(b) $\\alpha = 1$", bold=TRUE))

################################################################################
## Others ######################################################################
################################################################################

## Fig 4: Graph Anscombe plot
## save as 4 x 9 plot
colpal <- brewer.pal(n = 9, name = "Set1")[c(1,5,3,2,4,9)]
vertex.layout <- rbind(cbind(1.5*cos((0:8)*2*pi/9)-0.5, sin((0:8)*2*pi/9)),c(2,0))

g1 <- graph_from_edgelist(rbind(t(combn(9, 2)), c("0", 1)), directed = FALSE)
g2 <- graph_from_edgelist(rbind(matrix(c(1,rep(2:9, each=2), 1), byrow = TRUE, ncol = 2), 
                                c("0",1),c(1,5),c(1,6),c(2,6),c(2,7),
                                c(9,4),c(9,5),c(8,3),c(8,4),c(3,7)), directed = FALSE)
g3 <- graph_from_edgelist(rbind(matrix(c(1,rep(2:9, each=2), 1), byrow = TRUE, ncol = 2), 
                                c("0",1)), directed = FALSE)
# local L1 centrality of g1, g2, g3
# adding 0.1 to prevent rounding errors
do.call("cbind", L1centLOC(g1, alpha = pmin((3:10 + .1)/10,1))) -> l1g1
do.call("cbind", L1centLOC(g2, alpha = pmin((3:10 + .1)/10,1))) -> l1g2
do.call("cbind", L1centLOC(g3, alpha = pmin((3:10 + .1)/10,1))) -> l1g3

op <- par(oma = c(-0.1,0,0.1,0) + 0.1, las = 1)
nf <- layout(matrix(c(1,2,3, 4,5,6), ncol=3, byrow=FALSE))

# Fig 4 - G1
par(mar=c(0,0,0,0))
plot(g1, vertex.color = c(colpal[1:5],colpal[5:2],colpal[6]), 
     vertex.label.color = "white", vertex.label.family = "sans",
     layout = vertex.layout, vertex.size = 20, asp = 0.8)
text(1, 1, labels = TeX("$G_1$", bold = TRUE), font = 2, cex = 1.5)
par(mar=c(5,4,2,2) + 0.1)
plot(NA, xlim = c(0.3, 1), ylim = c(0,1), 
     xlab = TeX("\\alpha"), ylab = TeX("Local $L_1$ centrality"))
matlines((3:10)/10, t(l1g1[c(1,2,3,4,5,10),] + c(0,-(1:4)/100,0)), 
         col = colpal, lty = 1, lwd = 1.5)

# Fig 4 - G2
par(mar=c(0,0,0,0))
plot(g2, vertex.color = c(colpal[1:5],colpal[5:2],colpal[6]), 
     vertex.label.color = "white", vertex.label.family = "sans",
     layout = vertex.layout, vertex.size = 20, asp = 0.8)
text(1, 1, labels = TeX("$G_2$", bold = TRUE), font = 2, cex = 1.5)
par(mar=c(5,4,2,2) + 0.1)
plot(NA, xlim = c(0.3, 1), ylim = c(0,1), 
     xlab = TeX("\\alpha"), ylab = TeX("Local $L_1$ centrality"))
matlines((3:10)/10, t(l1g2[c(1,2,3,4,5,10),] + c(0,-(1:4)/100,0)), 
         col = colpal, lty = 1, lwd = 1.5)

# Fig 4 - G3
par(mar=c(0,0,0,0))
plot(g3, vertex.color = c(colpal[1:5],colpal[5:2],colpal[6]), 
     vertex.label.color = "white", vertex.label.family = "sans",
     layout = vertex.layout, vertex.size = 20, asp = 0.8)
text(1, 1, labels = TeX("$G_3$", bold = TRUE), font = 2, cex = 1.5)
par(mar=c(5,4,2,2) + 0.1)
plot(NA, xlim = c(0.3, 1), ylim = c(0,1), 
     xlab = TeX("\\alpha"), ylab = TeX("Local $L_1$ centrality"))
matlines((3:10)/10, t(l1g3[c(1,2,3,4,5,10),] + c(0,-(1:4)/100,0)), 
         col = colpal, lty = 1, lwd = 1.5)
par(op)


## Fig 2 (supplement): Well-known graphs
## save as 6 x 9 plot
n <- 10
g.complete <- make_full_graph(n)
g.cycle <- make_ring(n)
g.geodesic <- graph_from_edgelist(matrix(c(1,rep(2:(n-1),each=2),n),byrow=TRUE,ncol=2), 
                                  directed = FALSE)
g.star <- make_star(n, mode = "undirected")
g.tree <- make_tree(2^4-1, mode = "undirected")
g.grid <- make_lattice(c(3,4))

vertex.layout <- cbind(1.5*cos((0:(n-1))*2*pi/n)-0.5, sin((0:(n-1))*2*pi/n))
vertex.layout2 <- cbind(1.5*cos((0:(n-2))*2*pi/(n-1))-0.5, sin((0:(n-2))*2*pi/(n-1)))

par(mfrow = c(2,3))
par(mar=c(1,1,1,1) + 0.1)
# Fig 2(a) (supplement)
plot(g.complete, main="(a) Complete graph", layout = vertex.layout, 
     vertex.size = 25,
     vertex.color = rgb(0,0,0,alpha = L1cent(g.complete)), 
     vertex.label = L1cent(g.complete),
     vertex.label.color = "white",
     vertex.label.family="sans")
# Fig 2(b) (supplement)
plot(g.cycle, main="(b) Cycle graph", layout = vertex.layout, 
     vertex.size = 25,
     vertex.color = rgb(0,0,0,alpha = L1cent(g.cycle)), 
     vertex.label = L1cent(g.cycle),
     vertex.label.color = "white",
     vertex.label.family="sans")
# Fig 2(c) (supplement)
plot(g.geodesic, main="(c) Path graph", layout = cbind(rep(1:n),rep(1:n)), 
     vertex.size = 25,
     vertex.color = rgb(0,0,0,alpha = L1cent(g.geodesic)), 
     vertex.label = L1cent(g.geodesic),
     vertex.label.color = c(rep("black",2),rep("white",6),rep("black",2)),
     vertex.label.family="sans")
# Fig 2(d) (supplement)
plot(g.star, main="(d) Star graph", 
     vertex.color = rgb(0,0,0,alpha = L1cent(g.star)), 
     vertex.size = 25,
     vertex.label = L1cent(g.star),
     vertex.label.color = c(rep("white",1),rep("black",9)),
     vertex.label.family="sans")
# Fig 2(e) (supplement)
plot(g.tree, main="(e) Tree graph", layout = layout_as_tree(g.tree, root=1),
     vertex.size = c(25,rep(28,2),rep(25,12)),
     vertex.color = rgb(0,0,0,alpha = L1cent(g.tree)), 
     vertex.label = c(1,"14/15","14/15",rep(0.4,4),rep("2/15",8)),#L1cent(g.tree),
     vertex.label.color = c(rep("white",3),rep("black",12)),
     vertex.label.family="sans")
# Fig 2(f) (supplement)
plot(g.grid, main="(f) Regular grid graph",
     vertex.size = 25,
     vertex.color = rgb(0,0,0,alpha = L1cent(g.grid)), 
     vertex.label = c(rep(0.5,3),"2/3",1,rep("2/3",2),1,"2/3",rep(0.5,3)),#L1cent(g.grid),
     vertex.label.color = "white",
     vertex.label.family="sans")
par(mar=c(5,4,4,2) + 0.1)


## Fig 3 (supplement): Subgraph cannot be used for local L1 centrality computation
## save as 6 x 12 plot
g <- graph_from_edgelist(rbind(matrix(c(1,rep(2:9, each=2), 1), byrow = TRUE, ncol = 2), 
                               c("0",1),c(1,5),c(1,6),c(2,6),c(2,7),
                               c(9,4),c(9,5),c(8,3),c(8,4),c(3,7)), directed = FALSE)
vertex.layout <- rbind(cbind(1.5*cos((0:8)*2*pi/9)-0.5, sin((0:8)*2*pi/9)),c(2,0))
par(mfrow = c(1,2), mar = c(0,0,4.1,0))
# Fig 3(a) (supplement)
plot(g, vertex.label.color = "black", 
     vertex.color = "white",
     vertex.label.family = "sans",
     vertex.label = TeX(c(paste0("$v_",1:9,"$"),"$v_0$")),
     layout = vertex.layout, 
     vertex.size = 20, asp = 0.8,
     main = "(a) Graph G")
# Fig 3(b) (supplement)
plot(g, vertex.color = c("gray","black")[(L1centNB(g)[[5]] >= 0.475) + 1], 
     vertex.label.color = "white", 
     vertex.label.family = "sans",
     vertex.label = L1centNB(g)[[5]],
     layout = vertex.layout, 
     vertex.size = 20, asp = 0.8,
     main = TeX("(b) $L_1$ centrality in the graph symmetrized w.r.t. $v_5$", bold = TRUE))
par(mfrow = c(1,1), mar = c(5,4,4,2)+0.1)


## Section 4.1 of supplement: continuity of L1 centrality
# edge perturbation
MCUmovie.edge_perturb <- MCUmovie
wwg <- V(MCUmovie)$worldwidegross
E(MCUmovie.edge_perturb)$weight <- round(E(MCUmovie.edge_perturb)$weight, 4)
# upper bound
4 * min(max(distances(MCUmovie)), max(distances(MCUmovie.edge_perturb))) * 
  sum(abs(E(MCUmovie.edge_perturb)$weight - E(MCUmovie)$weight)) / 
  min(E(MCUmovie.edge_perturb)$weight) /
  min(E(MCUmovie)$weight)

(L1cent(MCUmovie, eta = wwg) - L1cent(MCUmovie.edge_perturb, eta = wwg)) |> abs() |> sort()
# no rank difference
(rank(L1cent(MCUmovie, eta = wwg)) - rank(L1cent(MCUmovie.edge_perturb, eta = wwg))) |> abs() |> sort()

# vertex perturbation
wwg <- V(MCUmovie)$worldwidegross
wwg.perturb <- round(V(MCUmovie)$worldwidegross/1e6)
# upper bound
sum(abs(wwg/sum(wwg) - wwg.perturb/sum(wwg.perturb)))

(L1cent(MCUmovie, eta = wwg) - L1cent(MCUmovie, eta = wwg.perturb)) |> abs() |> sort()
# no rank difference
(rank(L1cent(MCUmovie, eta = wwg)) - rank(L1cent(MCUmovie, eta = wwg.perturb))) |> abs() |> sort()


## Fig 4 (supplement): Counterexamples of monotonicity and neighborhood-inclusion axioms
## save as 4 x 9 plot
g.mn.counter <- graph_from_edgelist(matrix(c(1,4, 1,8, 2,4, 2,5, 2,7,
                                             3,6, 3,8, 3,9, 4,5, 4,9,
                                             5,9, 6,7, 6,9, 7,10, 8,9), byrow=TRUE, ncol=2), 
                                    directed = FALSE)
g.nb.counter <- graph_from_edgelist(matrix(c(1,3, 1,4, 1,5,
                                             2,3, 2,4, 2,5, 2,6,
                                             3,6, 3,8, 4,7, 4,9, 4,10,
                                             5,6, 5,10, 6,7, 6,8, 6,9), byrow=TRUE, ncol=2), 
                                    directed = FALSE)
set.seed(1)
ly <- layout_in_circle(g.nb.counter)

par(mfrow = c(1,2)) 
index <- c(5,7)
g.mn.counter.2 <- add_edges(g.mn.counter, index)
# Fig 4(a) (supplement)
plot(g.mn.counter.2, layout = ly, 
     vertex.label.color = "black", 
     vertex.color = "gray",
     vertex.label.family = "sans",
     vertex.label = TeX(paste0("$v_{",1:10,"}$")),
     layout = ly,
     vertex.size = 25, vertex.label.size = 30,
     edge.lty = c(rep(1,15),2),
     main = "(a) Score and rank monotonicity")
# Fig 4(b) (supplement)
plot(g.nb.counter, layout = ly, 
     vertex.label.color = "black", 
     vertex.color = "gray",
     vertex.label.family = "sans",
     vertex.label = TeX(paste0("$v_{",1:10,"}$")),
     vertex.size = 25,
     layout = ly,
     main = "(b) Neighborhood-inclusion condition")
par(mfrow = c(1,1)) 


## Table 2 (supplment): Comparing L1 centrality to the other measures
rank.diff <- matrix(0, ncol=5,nrow=5)
colnames(rank.diff) <- rownames(rank.diff) <- c("L1","Degree","Closeness","Betweenness","Eigen")

set.seed(0)
for(repeats in 1:100){
  while(TRUE){
    edgelist <- t(combn(100,2))[rbinom(4950,1,prob=1/10) == 1,]
    if(length(unique(c(edgelist))) == 100) break;
  }
  g.rnd <- graph_from_edgelist(edgelist, directed = FALSE)
  E(g.rnd)$weight <- runif(length(E(g.rnd)), 0.5, 1.5)
  l1 <- L1cent(g.rnd) |> rank()
  de <- degree(g.rnd) |> rank()
  cl <- closeness(g.rnd) |> rank()
  be <- betweenness(g.rnd) |> rank()
  ei <- eigen_centrality(g.rnd, weights = NA)$vector |> rank()
  rank.diff <- rank.diff + matrix(c(0, mean(abs(l1-de)), mean(abs(l1-cl)), mean(abs(l1-be)), mean(abs(l1-ei)),
                                    max(abs(l1-de)), 0, mean(abs(de-cl)), mean(abs(de-be)), mean(abs(de-ei)),
                                    max(abs(l1-cl)), max(abs(de-cl)), 0, mean(abs(cl-be)), mean(abs(cl-ei)),
                                    max(abs(l1-be)), max(abs(de-be)), max(abs(cl-be)), 0, mean(abs(be-ei)),
                                    max(abs(l1-ei)), max(abs(de-ei)), max(abs(cl-ei)), max(abs(be-ei)), 0), byrow = TRUE, ncol=5)
}
round(rank.diff/100,2) |> knitr::kable(format = "latex")
