library(xtable)
load("/FRG/db_nsdl/DOC/TABLES/DATA_VISUALISATION/survival_percentages.Rdata")
ls()

print(xtable(open.per, digits=1),
      hline.after=NULL, only.contents = TRUE,
      include.rownames = TRUE,
      include.colnames=FALSE,
      type="latex",
      file="/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/survival_percentages.tex")


