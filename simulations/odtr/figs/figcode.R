library(ggpubr)

load(file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/odtr/figs/ODTR_complex_figs.RData")
load(file = "/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/odtr/figs/ODTR_simple_figs.RData")

odtr_plot_complex_n1000 = odtr_plot_complex_n1000 + theme(text = element_text(size=18))
odtr_plot_complex_n300 = odtr_plot_complex_n300 + theme(text = element_text(size=18))
odtr_plot_simple_n1000 = odtr_plot_simple_n1000 + theme(text = element_text(size=18))
odtr_plot_simple_n300 = odtr_plot_simple_n300 + theme(text = element_text(size=18))

png(filename="/Users/linamontoya/Box Sync/Dissertation/SL.ODTR/simulations/odtr/figs/fig_paper.png", width = 1300, height = 850)
ggarrange(odtr_plot_complex_n1000, odtr_plot_complex_n300, odtr_plot_simple_n1000, odtr_plot_simple_n300, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

dev.off()
