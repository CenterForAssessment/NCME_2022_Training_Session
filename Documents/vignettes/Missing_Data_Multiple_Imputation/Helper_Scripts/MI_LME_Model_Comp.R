###   Coalate and print model (fixed effects) estimate comparison
tmod <- data.table(round(summary(true_mod)$coefficients, 2), keep.rownames = T, key = "rn")
mmod <- data.table(round(summary(miss_mod)$coefficients, 2), keep.rownames = T, key = "rn")
imod <- data.table(modelFixedEff(imp_mods))

sepr <- "-------"
comp_table <- data.table(
    ` `=c("Fixed Effects", "  Coefficients", "-----------------", imod$term), `|` = "|",
    ` `=c(sepr," True ",sepr,tmod[[2]]), `ESTIMATE`=c(sepr,"Missing",sepr,mmod[[2]]), ` `=c(sepr,"Imputed",sepr,round(imod[[2]], 2)), `|` = "|",
    ` `=c(sepr," True ",sepr,tmod[[3]]), `STD ERR`=c(sepr,"Missing",sepr,mmod[[3]]), ` `=c(sepr,"Imputed",sepr,round(imod[[3]], 2)), `|` = "|",
    ` `=c(sepr," True ",sepr,tmod[[4]]), `t VALUE`=c(sepr,"Missing",sepr,mmod[[4]]), ` `=c(sepr,"Imputed",sepr,round(imod[[4]], 2)), `|` = "|")

cat("\n\n")
print(comp_table, class = F, row.names = F)
cat("\n")
