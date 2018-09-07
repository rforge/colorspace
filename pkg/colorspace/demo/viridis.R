if(requireNamespace("viridis")) {

library("colorspace")

specplot(viridis::viridis(9), sequential_hcl(9, "Viridis"), main = "Viridis", rgb = FALSE)
specplot(viridis::plasma(9), sequential_hcl(9, "Plasma"), main = "Plasma", rgb = FALSE)
specplot(viridis::inferno(9), sequential_hcl(9, "Inferno"), main = "Inferno", rgb = FALSE)

}
