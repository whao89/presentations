setwd("/home/whao/git/popgen/tex/figures/TGP")

library(ggplot2)
library(gtable)
library(gridExtra)
library(RColorBrewer)
source("http://www.princeton.edu/~whao/theme_complete_bw.R")

PALETTE = brewer.pal(9,"Set1")

#make K=7
IND = c(7,3,1,5,2,4,6)

prob=read.table("theta_7.txt")
d = ncol(prob)-3
prob=prob[,3:(3+d-1)]
prob=as.matrix(prob[,IND])
colnames(prob)=NULL


load("TGP_1718_info.Rdata")

#drop CHD
IND = tgp_geo_fine != "CHD"
prob = prob[IND,]
tgp_geo_fine = tgp_geo_fine[IND]
tgp_geo_coarse = tgp_geo_coarse[IND]

prob = as.vector(prob)
indv = as.factor(rep(1:1717, d))
pops = as.factor(sort(rep(1:d, 1717)))
geo_coarse  = as.factor(rep(tgp_geo_coarse, d))



fine_order = split(tgp_geo_fine, tgp_geo_coarse)
for(coarse in names(fine_order)){
    fine_order[[coarse]] = sort(fine_order[[coarse]])
}

geo_fine = factor(rep(tgp_geo_fine, d), levels=unique(unlist(fine_order)))

dat = as.data.frame(prob)
dat = cbind(dat, indv, pops, geo_coarse, geo_fine)

p7 = ggplot(dat, aes(x=indv, y=prob, fill=pops)) +
     geom_bar(stat="identity") +
     facet_grid(. ~ geo_fine, space="free", scales="free") +
     theme_complete_bw(16) +
     scale_fill_manual(name="", values=PALETTE[1:d]) +
     theme(axis.text.y = element_blank()) +
     theme(axis.text.x = element_blank()) +
     theme(strip.text.x = element_text(angle = 90)) +
     theme(panel.border = element_rect(colour = NA)) +
     theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
     theme(panel.grid.major = element_line(colour = NA)) +
     theme(panel.grid.minor = element_line(colour = NA)) +
     theme(axis.ticks = element_line(colour = NA))+
     theme(legend.key = element_blank())

#make K=8
IND = c(5,6,2,7,4,8,3,1)

prob=read.table("theta_8.txt")
d = ncol(prob)-3
prob=prob[,3:(3+d-1)]
prob=as.matrix(prob[,IND])
colnames(prob)=NULL


load("TGP_1718_info.Rdata")

#drop CHD
IND = tgp_geo_fine != "CHD"
prob = prob[IND,]
tgp_geo_fine = tgp_geo_fine[IND]
tgp_geo_coarse = tgp_geo_coarse[IND]

prob = as.vector(prob)
indv = as.factor(rep(1:1717, d))
pops = as.factor(sort(rep(1:d, 1717)))
geo_coarse  = as.factor(rep(tgp_geo_coarse, d))



fine_order = split(tgp_geo_fine, tgp_geo_coarse)
for(coarse in names(fine_order)){
    fine_order[[coarse]] = sort(fine_order[[coarse]])
}

geo_fine = factor(rep(tgp_geo_fine, d), levels=unique(unlist(fine_order)))

dat = as.data.frame(prob)
dat = cbind(dat, indv, pops, geo_coarse, geo_fine)

p8 = ggplot(dat, aes(x=indv, y=prob, fill=pops)) +
     geom_bar(stat="identity") +
     facet_grid(. ~ geo_fine, space="free", scales="free") +
     theme_complete_bw(16) +
     scale_fill_manual(name="", values=PALETTE[1:d]) +
     theme(axis.text.y = element_blank()) +
     theme(axis.text.x = element_blank()) +
     theme(strip.text.x = element_text(angle = 90)) +
     theme(panel.border = element_rect(colour = NA)) +
     theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
     theme(panel.grid.major = element_line(colour = NA)) +
     theme(panel.grid.minor = element_line(colour = NA)) +
     theme(axis.ticks = element_line(colour = NA))+
     theme(legend.key = element_blank())


#make K=9
IND = c(1,2,3,8,5,4,7,9,6)

prob=read.table("theta_9.txt")
d = ncol(prob)-3
prob=prob[,3:(3+d-1)]
prob=as.matrix(prob[,IND])
colnames(prob)=NULL


load("TGP_1718_info.Rdata")

#drop CHD
IND = tgp_geo_fine != "CHD"
prob = prob[IND,]
tgp_geo_fine = tgp_geo_fine[IND]
tgp_geo_coarse = tgp_geo_coarse[IND]

prob = as.vector(prob)
indv = as.factor(rep(1:1717, d))
pops = as.factor(sort(rep(1:d, 1717)))
geo_coarse  = as.factor(rep(tgp_geo_coarse, d))



fine_order = split(tgp_geo_fine, tgp_geo_coarse)
for(coarse in names(fine_order)){
    fine_order[[coarse]] = sort(fine_order[[coarse]])
}

geo_fine = factor(rep(tgp_geo_fine, d), levels=unique(unlist(fine_order)))

dat = as.data.frame(prob)
dat = cbind(dat, indv, pops, geo_coarse, geo_fine)

p9 = ggplot(dat, aes(x=indv, y=prob, fill=pops)) +
     geom_bar(stat="identity") +
     facet_grid(. ~ geo_fine, space="free", scales="free") +
     theme_complete_bw(16) +
     scale_fill_manual(name="", values=PALETTE[1:d]) +
     theme(axis.text.y = element_blank()) +
     theme(axis.text.x = element_blank()) +
     theme(strip.text.x = element_text(angle = 90)) +
     theme(panel.border = element_rect(colour = NA)) +
     theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
     theme(panel.grid.major = element_line(colour = NA)) +
     theme(panel.grid.minor = element_line(colour = NA)) +
     theme(axis.ticks = element_line(colour = NA))+
     theme(legend.key = element_blank())


####################
pdf("TGP_7.pdf", width=10, height=6)
p7
dev.off()
pdf("TGP_8.pdf", width=10, height=6)
p8
dev.off()
pdf("TGP_9.pdf", width=10, height=6)
p9
dev.off()

