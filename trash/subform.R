
# h/t https://github.com/tidyverse/glue/issues/108
subform <- function(formula, ...){
  args <- syms(list(...))
  subst_ <- substitute(
    substitute(formula, args),
    list(formula = formula))
  as.formula(eval(subst_),attr(formula,".Environment"))
}


# h/t https://github.com/tidyverse/glue/issues/108
#formula5 <- subform(x ~ B2+B3+B4+B5+B6+B7+B8+B8A +NDWI+NDCI+NDTI+NDMI+MNDWI+NDVI,  
#x = names(df_wide_c_model)[i])