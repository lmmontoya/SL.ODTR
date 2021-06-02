library(ggpubr)

load(file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/odtr/figs/ODTR_complex_figs.RData")
load(file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/odtr/figs/ODTR_simple_figs.RData")

odtr_plot_complex_n1000 = odtr_plot_complex_n1000 + theme(text = element_text(size=18)) + ylab("") + ggtitle("A: DGP 1, \"Complex\" Blip, n = 1,000")
odtr_plot_complex_n300 = odtr_plot_complex_n300 + theme(text = element_text(size=18)) + ggtitle("B: DGP 1, \"Complex\" Blip, n = 300")
odtr_plot_simple_n1000 = odtr_plot_simple_n1000 + theme(text = element_text(size=18)) + ylab("") + ggtitle("C: DGP 2, \"Simple\" Blip, n = 1,000")
odtr_plot_simple_n300 = odtr_plot_simple_n300 + theme(text = element_text(size=18)) + ylab("") + ggtitle("D: DGP 2, \"Simple\" Blip, n = 300")

png(filename="/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/odtr/figs/fig_paper.png", width = 1000, height = 1500)
ggarrange(odtr_plot_complex_n1000, odtr_plot_complex_n300, odtr_plot_simple_n1000, odtr_plot_simple_n300, ncol=1, nrow=4, common.legend = TRUE, legend="bottom")

dev.off()
