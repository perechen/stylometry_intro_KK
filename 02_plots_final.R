library(tidyverse)
library(patchwork)
library(RColorBrewer)


### Fig 1.
style_master = list(theme_minimal(), theme(axis.text = element_text(size=10),axis.title = element_text(size=12)), scale_color_grey(start=0.4,end=0.8), scale_fill_grey(start=0.4,end=0.8))

load("data/simple_features_plots.rda")

p_sent = p_sent + 
  guides(color=F) + 
  labs(subtitle = "a.",
       x = "lausepikkused sõnades",
       y = "lausete arv") +
  style_master

p_fword = p_fword +
  guides(color=F) +
  labs(subtitle = "c.",
       x = "'ja' esinemissagedus",
       y = "'ta' esinemissagedus") +
  style_master

p_lenword = p_lenword + 
  guides(color=guide_legend(title="autor")) +
  labs(subtitle = "b.",
       x = "sõnapikkused tähemärkides",
       y = "sõnade arv") +
  style_master

p_lett= p_lett + 
  guides(color=guide_legend(title="autor")) +
  labs(subtitle = "d.",
                      x = "o-tähe esinemissagedus",
                      y = "a-tähe esinemissagedus") +
  style_master

p_sent + p_lenword + p_fword + p_lett + 
  plot_layout(ncol = 2)

ggsave("greyscale_plots/Fig1_grey.png",device="png",width = 8)
ggsave("greyscale_plots/Fig1_grey.tiff",device="tiff",width = 8)


### Fig 2.

load("data/diff_example_plots.rda")

p1 = p_rel + style_master + theme(axis.text.x = element_text(angle=-90)) + labs(x="20 sõna sageduse järjekorras",y="suhteline sagedus") 
p2 = p_z + style_master + theme(axis.text.x = element_text(angle=-90)) + guides(fill=guide_legend(title="autor")) + labs(x="20 sõna sageduse järjekorras",y="standardiseeritud sagedused (z-skoorid)") 

summary(p2)
p1 + p2
#fig.height=4,fig.width=9

ggsave("greyscale_plots/Fig2_grey.png",device="png",height = 4,width = 9)
ggsave("greyscale_plots/tiffs/Fig2_grey.tiff",device="tiff",height = 4,width = 9)


### Fig 3.

load("data/man_viz_p.rda")

man_viz_p + style_master + guides(color=guide_legend(title="autor",)) + scale_color_manual(labels=c("eukleidiline", "Manhattan"), values=c("#CCCCCC", "#666666"))

#fig.width=8, fig.height=6
ggsave("greyscale_plots/Fig3_grey.png",device="png",height = 6,width = 8)
ggsave("greyscale_plots/tiffs/Fig3_grey.tiff",device="tiff",height = 6,width = 8)


### Fig 4.

library(ggdendro)
load("data/dist_viz.rda")

p_mds1 = p_mds + style_master + guides(color=F) + theme(axis.title = element_blank()) + xlim(-1.4,1.4) + ylim(-0.6,0.6) + labs(title="b")

p_dendro1 = p_dendro + style_master + theme(axis.text.x=element_blank(),
                                            axis.title = element_blank(),panel.grid.major= element_blank(),
                                            panel.grid.minor=element_blank()) + labs(title="a")

p_graph1 = p_graph + style_master + theme_void() +
  #scale_x_continuous(expand=c(-2,2.3)) +
  #scale_y_continuous(expand=c(0,0.2)) +
  labs(title="c") + xlim(-0.3,1.2) + ylim(-0.2,1.2)

p_dendro1 + p_mds1 + p_graph1

#,fig.width=9, fig.height=4
ggsave("greyscale_plots/Fig4_grey.png",device="png",height = 4.5,width = 9)
ggsave("greyscale_plots/tiffs/Fig4_grey.tiff",device="tiff",height = 6,width = 9)

### Fig 5.

load("data/res_words.rda")
load("data/results_samples.rda")

methods_est <- c(
  `Burrows' Delta` = "Burrowsi delta",
  `Cosine` = "Koosinus",
  `Cosine Delta` = "Koosinusdelta",
  `Euclidean` = "eukleidiline",
  `Manhattan` = "Manhattan"
)


results_words %>% 
  ggplot(aes(mfw, accuracy)) + 
  geom_line(size=1) + 
  facet_wrap(~method, ncol=5,labeller = as_labeller(methods_est)) + 
  labs(title="",
       x="Analüüsitud kõige sagedasemate sõnade arv",
       y="täpsus") + scale_x_log10(breaks=c(10,100,1000,10000)) + 
  theme_bw() + 
  style_master +   theme(legend.position = "top",
                         strip.text=element_text(size=14),
                         axis.title = element_text(size=12),
                         plot.title = element_text(size=14),
                         axis.text = element_text(size=8)) + guides(linetype=guide_legend(title=NULL))

#fig.height=3, fig.width=8
ggsave("greyscale_plots/Fig5_grey.png",device="png",height = 3,width = 8)
ggsave("greyscale_plots/tiffs/Fig5_grey.tiff",device="tiff",height = 3,width = 8)


### Fig 6.


#fig.height=4, fig.width=6

results_samples %>% group_by(sample_size) %>% mutate(upper_ci = Rmisc::CI(accuracy)[1],
                                                     lower_ci = Rmisc::CI(accuracy)[3]) %>% 
  
  group_by(sample_size,upper_ci,lower_ci) %>% summarise(mean_acc = mean(accuracy)) %>% 
  ggplot(aes(sample_size, mean_acc)) + 
  geom_line(size=1) +
#  geom_ribbon(aes(ymin=lower_ci,ymax=upper_ci),alpha=0.2) +
  labs(title="", x="Valimi suurus sõnades", y="Autorituvastuse täpsus") +
  theme_minimal() + theme(
                          axis.title = element_text(size=12),
                          plot.title = element_text(size=14),
                          axis.text = element_text(size=10)) +
  scale_x_continuous(breaks = seq(0, 10000, length.out=5))


ggsave("greyscale_plots/Fig6_grey.png",device="png",height = 4,width = 6)
ggsave("greyscale_plots/tiffs/Fig6_grey.tiff",device="tiff",height = 4,width = 6)
