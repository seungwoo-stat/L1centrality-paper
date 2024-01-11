library(igraph)
library(latex2exp)
library(L1centrality)
library(ggplot2)
library(ggrepel)

################################################################################
## MCU movie network ###########################################################
################################################################################

## Fig 1: L1 centrality of MCU movie network
## save as 4 x 12 plot
df <- data.frame(y=rep(0,32),x=L1cent(MCUmovie, eta = V(MCUmovie)$worldwidegross))
rownames(df) <- V(MCUmovie)$name
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


## Fig 2: MCU movie network's Fruchterman-Reingold plot, Target plot
## save as 6 x 12 plot
set.seed(0)
params <- L1centMDS(MCUmovie)
par(mfrow=c(1,2), mar = c(5, 4, 4, 2) + 0.1)
plot(MCUmovie, layout = layout_with_fr, vertex.size = c(rep(5,18),7,rep(5,13)),
     vertex.color=c(rep("gray",18),"black",rep("gray",13)),
     vertex.label.cex = 0.6, vertex.label.dist = 0, vertex.label.color="black",
     main="(a) Fruchterman-Reingold layout plot", vertex.label.family = "sans")
plot(params, main="(b) Target plot", plot.col="black",
     plot.cex=1.5, plot.pch=21, plot.bg=c(rep("gray",18),"black",rep("gray",13)))


## Fig 3 is not drawn with R.


## Fig 4: L1 centrality of MCU movie network symmetrized w.r.t. `Spider-Man: No
## Way Home`
## save as 4 x 12 plot
df <- data.frame(y=rep(0,32),x=L1centNB(MCUmovie, eta = V(MCUmovie)$worldwidegross)$`Spider-Man: No Way Home`)
rownames(df) <- V(MCUmovie)$name
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


## Fig 5: multiscale edge representation of MCU movie network
## save as 4 x 12 plot
MCU.edge <- L1centEDGE(MCUmovie, eta = V(MCUmovie)$worldwidegross, alpha = (2:32)/32)
MCU.edge[[7]] |> graph_from_edgelist(directed=TRUE) -> g8
MCU.edge[[15]] |> graph_from_edgelist(directed=TRUE) -> g16
MCU.edge[[31]] |> graph_from_edgelist(directed=TRUE) -> g32
mculabel8 <- V(g8)$name
mculabel8[!mculabel8 %in% c("Avengers: Endgame","Avengers: Infinity War")] <- NA
mculabel16 <- V(g16)$name
mculabel16[!mculabel16 %in% c("Avengers: Endgame","Avengers: Infinity War",
                              "Ant-Man and the Wasp","Ant-Man and the Wasp: Quantumania")] <- NA
mculabel32 <- V(g32)$name
mculabel32[!mculabel32 %in% c("Avengers: Infinity War")] <- NA

par(mfrow=c(1,3))
set.seed(0)
plot(g8,vertex.color="gray",vertex.label.cex = 1,vertex.label.dist = 0,
     vertex.label.color="black",
     vertex.size=sapply(V(g8)$name,\(n)ifelse(n %in% c("Avengers: Infinity War", "Avengers: Endgame"), 15, 7)),
     vertex.label.family = "sans",edge.arrow.size=0.5,
     main=TeX("(a) $\\alpha$=8/32",bold=TRUE),vertex.label=mculabel8)
plot(g16,vertex.color="gray",vertex.label.cex = 1,vertex.label.dist = 0,
     vertex.label.color="black",
     vertex.size=sapply(V(g16)$name,\(n)ifelse(n %in% c("Avengers: Infinity War", "Avengers: Endgame"), 15, 7)),
     vertex.label.family = "sans",edge.arrow.size=0.5,
     main=TeX("(b) $\\alpha$=16/32",bold=TRUE),vertex.label=mculabel16)
plot(g32,vertex.color="gray",vertex.label.cex = 1,vertex.label.dist = 0,
     vertex.label.color="black",
     vertex.size=sapply(V(g32)$name,\(n)ifelse(n %in% c("Avengers: Infinity War"), 15, 7)),
     vertex.label.family = "sans",edge.arrow.size=0.5,
     main=TeX("(c) $\\alpha$=32/32",bold=TRUE),vertex.label=mculabel32)


## Fig 6: Lorenz curve of MCU movie network
## save as 8 x 8 plot
par(mfrow=c(1,1))
L1cent(MCUmovie) |> Lorenz_plot(asp=1, main="Lorenz plot of MCU movie network") -> gini1
L1cent(MCUmovie, eta = V(MCUmovie)$worldwidegross) |> Lorenz_plot(add=TRUE, lty=2) -> gini2
L1cent(MCUmovie, eta = 1/V(MCUmovie)$worldwidegross) |> Lorenz_plot(add=TRUE, lty=4) -> gini3
legend("topleft", lty=c(0,1,2,4), legend=c(as.expression(bquote(bold("Multiplicity"))),
                                           "Equal","Worldwide gross","1/(Worldwide gross)"),bty="n")
c(gini1, gini2, gini3) |> round(4)


################################################################################
## assembly network ############################################################
################################################################################

color.party <- c("black","#004EA1","black","#FFED00","#E61E2B","black","black")
data(rokassembly21)
# D2, D181: Chairmen of the National Assembly
rok.reduce <- delete_vertices(rokassembly21, c(V(rokassembly21)$name[!V(rokassembly21)$full], "D2", "D181"))

## Fig 7: assembly network - Target plot + global vs. local (alpha = 15/279)
## save as 8 x 17 plot
rok.global <- L1cent(rok.reduce)
rok.local <- L1centLOC(rok.reduce, alpha = 15/279)

opar <- par(oma=c(0,0,0,9.5),mar=c(5,4,4,1)+0.1,mfrow=c(1,2))
set.seed(0)
params.rok.full <- L1centMDS(rokassembly21)
plot(params.rok.full, plot.bg=color.party[V(rokassembly21)$party],
     plot.pch=sapply(V(rokassembly21)$full,\(f)ifelse(f,21,4)),
     text.labels="", main="(a) Target plot of 317 members",
     plot.col=sapply(1:317,\(n)ifelse(V(rokassembly21)$full[n],"black",color.party[V(rokassembly21)$party[n]])))
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

# number of cosponsored bills of each member of the Justice Party
sapply(paste0("J",1:6), \(n) V(rok.reduce)$nbill[V(rok.reduce)$name==n])

# distribution of cosponsored bills of two members in the reduced assembly network
(1/(as_adjacency_matrix(rok.reduce, attr="weight"))) |> as.vector() -> cospon.dist
cospon.dist[cospon.dist == Inf] <- NA
summary(cospon.dist)
ecdf(cospon.dist)(416)

# cosponsored bills of each Justice Party member with the other memeber in the
# reduced assembly network: top 6.
lapply(paste0("J",1:6),
       \(n){
         temp <- (1/(as_adjacency_matrix(rok.reduce, attr="weight"))[n,]) #/V(rok.reduce)$nbill[V(rok.reduce)$name==n]
         temp[temp != Inf] |>
         sort(decreasing = TRUE) |>
         head(6)
         })

# distribution of cosponsored bill by each member in the reduced assembly network
summary(V(rok.reduce)$nbill)


## Analysis of the P90, P57 and Fig. 8 (8 x 8 plot)
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
between.table[order(between.table$weight,decreasing = TRUE)[1:10],]

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


## Fig 9: Functional data set extracted from the reduced assembly network
## save as 8 x 8 plot
rok.local.all <- L1centLOC(rok.reduce,alpha=seq(5,275,by=5)/279)
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


## Fig 10: Multiscale edge reprsentation of the reduced assembly network
## save as 8 x 17 plot
edges <- L1centEDGE(rok.reduce, alpha = c(15/279,279/279))
edges[[1]] |> graph_from_edgelist() -> g15
edges[[2]] |> graph_from_edgelist() -> g279
localmeds15 <- unique(edges[[1]][,2])
localmeds279 <- unique(edges[[2]][,2])

set.seed(0)
par(mfrow=c(1,2))
g15 |> plot(layout = layout_with_kk,
            vertex.color=sapply(V(g15)$name,\(n)color.party[V(rok.reduce)$party[V(rok.reduce)$name==n]]),
            vertex.label.cex = 1,vertex.label.dist = 0,
            vertex.label.color=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds15,"white","black") ),
            vertex.size=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds15,15,3) ),
            vertex.label.family = "sans",edge.arrow.size=0.5,
            vertex.label = sapply(V(g15)$name,\(n)ifelse(n %in% localmeds15, n, NA)),
            main=TeX("(a) $\\alpha = 15/279$", bold=TRUE))
g279 |> plot(layout = layout_with_fr,
             vertex.color=sapply(V(g15)$name,\(n)color.party[V(rok.reduce)$party[V(rok.reduce)$name==n]]),
             vertex.label.cex = 1,vertex.label.dist = 0,
             vertex.label.color=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds279,"white","black") ),
             vertex.size=sapply(V(g15)$name,\(n)ifelse(n %in% localmeds279,15,3) ),
             vertex.label.family = "sans",edge.arrow.size=0.5,
             vertex.label = sapply(V(g15)$name,\(n)ifelse(n %in% localmeds279, n, NA)),
             main=TeX("(b) $\\alpha = 1$", bold=TRUE))


################################################################################
## Appendix B ##################################################################
################################################################################

l1 <- L1cent(MCUmovie)
MCUmovie.no.wt <- MCUmovie
E(MCUmovie.no.wt)$weight <- rep(1,278)
l1.no.wt <- L1cent(MCUmovie.no.wt)
deg <- degree(MCUmovie)
bet <- betweenness(MCUmovie)
clo <- closeness(MCUmovie)
par(mfcol=c(3,2))
plot(l1, deg, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Degree centrality")
plot(l1, bet, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Betweeness centrality")
plot(l1, clo, pch=20, xlab = TeX("$L_1$ centrality"), ylab="Closeness centrality")
plot(ecdf(l1)(l1), ecdf(deg)(deg), pch=20, xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Degree centrality (uniform margin)")
plot(ecdf(l1)(l1), ecdf(bet)(bet), pch=20, xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Betweeness centrality (uniform margin)")
plot(ecdf(l1)(l1), ecdf(clo)(clo), pch=20, xlab = TeX("$L_1$ centrality (uniform margin)"), ylab="Closeness centrality (uniform margin)")
cor(l1, deg); cor(l1, bet); cor(l1, clo)
cor(l1, deg, method="spearman"); cor(l1, bet, method="spearman"); cor(l1, clo, method="spearman")
