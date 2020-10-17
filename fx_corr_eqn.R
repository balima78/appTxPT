# função para calcular coeficiente de correlação e valor de p
corr_eqn <- function(x,y, method='pearson', digits = 3) {
  corr_coef <- round(cor.test(x, y, method=method)$estimate, digits = digits)
  corr_pval <- round((cor.test(x,y, method=method)$p.value), digits = digits)
  paste(method, 'r = ', corr_coef, ',', 'pval =', corr_pval)
}