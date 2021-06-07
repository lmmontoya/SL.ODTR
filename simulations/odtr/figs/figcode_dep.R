library(ggpubr)

load(file = "/Users/lmontoya/Box/SL.ODTR/simulations/odtr/figs/ODTR_dep_figs.RData")

odtr_plot_dep_n1000 = odtr_plot_dep_n1000 + theme(text = element_text(size=18)) + ylab("") + ggtitle("A: DGP 3, \"Complex\" Blip with Dependence in Covariates, n = 1,000")
odtr_plot_dep_n300 = odtr_plot_dep_n300 + theme(text = element_text(size=18)) + ggtitle("B: DGP 3, \"Complex\" Blip with Dependence in Covariates, n = 300")

png(filename="/Users/lmontoya/Box/SL.ODTR/simulations/odtr/figs/fig_paper_dep.png", width = 1000, height = 1500)
ggarrange(odtr_plot_dep_n1000, odtr_plot_dep_n300, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")

dev.off()
