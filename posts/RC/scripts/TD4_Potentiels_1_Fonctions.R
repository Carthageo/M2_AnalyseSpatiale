library(potential)

#### Fun = e

old_par <- par()
par(mfrow = c(2, 2))

png(filename = "img/potential_function_e_beta.png", width = 30, height = 20, units = "cm", res = 150)
layout(mat=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T))
for (beta in c(0,1,2,10)){
  plot_inter(fun = "e", span = 300, beta = beta, limit = 2000)
}
dev.off()


png(filename = "img/potential_function_e_span.png", width = 30, height = 20, units = "cm", res = 150)
layout(mat=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T))
for (span in c(100,200,300,400)){
  plot_inter(fun = "e", span = span, beta = 2, limit = 2000)
}
dev.off()



png(filename = "img/potential_function_power_beta.png", width = 30, height = 20, units = "cm", res = 150)
layout(mat=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T))
for (beta in c(0,1,2,10)){
  plot_inter(fun = "p", span = 300, beta = beta, limit = 2000)
}
dev.off()


png(filename = "img/potential_function_power_span.png", width = 30, height = 20, units = "cm", res = 150)
layout(mat=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T))
for (span in c(100,200,300,400)){
  plot_inter(fun = "p", span = span, beta = 2, limit = 2000)
}
dev.off()
