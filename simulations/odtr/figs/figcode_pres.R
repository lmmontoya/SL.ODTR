library(ggpubr)

load(file = "/Users/linamontoya/Box Sync/SL.ODTR/simulations/odtr/figs/ODTR_complex_figs.RData")
load(file = "/Users/linamontoya/Box Sync/SL.ODTR/simulations/odtr/figs/ODTR_simple_figs.RData")

odtr_plot_complex_n1000_pres = odtr_plot_complex_n1000_pres + theme(text = element_text(size=18))
odtr_plot_simple_n1000_pres = odtr_plot_simple_n1000_pres + theme(text = element_text(size=18))

png(filename="/Users/linamontoya/Box Sync/SL.ODTR/simulations/odtr/figs/fig_pres_complex.png", width = 1200, height = 800)
ggarrange(odtr_plot_complex_n1000_pres, ncol=1, nrow=1)
dev.off()

png(filename="/Users/linamontoya/Box Sync/SL.ODTR/simulations/odtr/figs/fig_pres_simple.png", width = 1200, height = 800)
ggarrange(odtr_plot_simple_n1000_pres, ncol=1, nrow=1)
dev.off()
