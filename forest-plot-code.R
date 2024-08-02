library(readxl) #Only necessary if your datasets are in Excel format
library(metafor)

#Meta-analysis forest plot (meta-analysis and subgroup without McKetin)
meta_fp <- read_excel("/your/data/here")
res <- rma(ES, sei = se, data=meta_fp)
mlabfun <- function(text, x) {
  list(bquote(paste(.(text), 
                    "; ", I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    "df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
}
forest(res, xlim=c(-16, 6), at=c(-1.5, 0, 1.5), ilab=cbind(meta_fp$Weight), ilab.xpos=c(-4.5),
       cex=0.75, ylim=c(-4, 25), order=alloc, rows=c(13:21, 2:9),
       psize=1, slab=meta_fp$StudyID)
op <- par(cex=0.75, font=2)
par(cex=0.75, font=4)
text(-16, c(22, 10), pos=4, c("1.1 Meta-analysis", "1.2 Subgroup analysis"))
par(op)
res.r <- rma(ES, sei = se, subset=(alloc=="Sensitivity analysis"), data=meta_fp)
res.a <- rma(ES, sei = se, subset=(alloc=="Meta-analysis"), data=meta_fp)
addpoly(res.r, row=0.5, mlab=mlabfun("RE Model for Subgroup analysis", res.r))
addpoly(res.a, row=11.5, mlab=mlabfun("RE Model for Meta-analysis", res.a))
res <- rma(ES, sei = se, mods = ~ alloc, data=meta_fp)
text(-16, -2, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                            "df = ", .(res$p - 1), ", ",
                                            .(fmtp(res$QMp, digits=2, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
text(-16, 23.5, "Study", pos=4, font=2, cex=.8)
text(6, 23.5, "SMD [95% CI]", pos=2, font=2, cex=.8)
text(c(-4.5), 23.5, c("Weight (%)"), font=2, cex=.8)

#RR forest plot
RR_fp <- read_excel("your/data/here")
RR_calc <- escalc(measure = "RR", ai = ae_pos_e, bi = ae_neg_e, ci = ae_pos_p, di = ae_neg_p, data = RR_fp, append = TRUE)
RR_SA <- rma(yi = yi, sei = se, data = RR_calc)

forest(RR_SA, xlim=c(-16, 5), atransf = exp, at=log(c(0.25, 1, 2)), ilab=cbind(RR_fp$ae_pos_p, RR_fp$total1, RR_fp$ae_pos_e, RR_fp$total2, RR_fp$Weight), ilab.xpos=c(-10.7,-9.5,-7.7,-6.5,-2.5),
       cex=0.75, ylim=c(-1, 8), rows=c(1:5),
       psize=1, slab = RR_fp$StudyID)
op <- par(cex=0.75, font=2)
par(cex=0.75, font=4)
text(-16, c(22), pos=4, c("Risk Ratio SA"))
par(op)
res.r3 <- rma(ES, sei = se, data=RR_fp)
res3 <- rma(ES, sei = se, data=RR_fp)
text(-16, 6.5, "Study", pos=4, font=2, cex=.8)
text(c(-10.7,-9.5,-7.7,-6.5), 6.5, c("Events", "Total", "Events", "Total"),   font=2, cex=.8)
text(5, 6.5, "logRR [95% CI]", pos=2, font=2, cex=.8)
text(c(-2.5), 6.5, c("Weight (%)"),font=2, cex=.8)
text(c(-10,-7), 7.2, c("Placebo", "NAC"),   font=2, cex=.8)
par(op)
text(-16, -1, pos=4, cex=0.75, bquote(paste(
  "                 (df = ", .(RR_SA$k - RR_SA$p), ", ",
  .(fmtp(RR_SA$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
  I^2, " = ", .(fmtx(RR_SA$I2, digits=1)), "%)")))

#Alcohol forest plot
alcohol <- read_excel("/your/data/here")
res4 <- rma(ES, sei = se, data=alcohol)
mlabfun <- function(text, x) {
  list(bquote(paste(.(text), 
                    "; ", I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    "df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
}
forest(res4, xlim=c(-16, 6), at=c(-1.5, 0, 1.5), ilab=cbind(alcohol$Weight), ilab.xpos=c(-4.5),
       cex=0.75, ylim=c(-4, 17), order=alloc, rows=c(11:13, 2:7),
       psize=1, slab=alcohol$StudyID)
op <- par(cex=0.75, font=2)
par(cex=0.75, font=4)
text(-16, c(8, 14), pos=4, c("2.2 All other trials", "2.1 Alcohol trials"))
par(op)
res.r4 <- rma(ES, sei = se, subset=(alloc=="other"), data=alcohol)
res.a4 <- rma(ES, sei = se, subset=(alloc=="alcohol"), data=alcohol)
addpoly(res.r4, row=0.7, mlab=mlabfun("RE Model for All other trials", res.r4))
addpoly(res.a4, row=9.7, mlab=mlabfun("RE Model for Alcohol trials", res.a4))
res4 <- rma(ES, sei = se, mods = ~ alloc, data=alcohol)
text(-16, -2, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                            "df = ", .(res4$p - 1), ", ",
                                            .(fmtp(res4$QMp, digits=2, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
text(-16, 15.5, "Study", pos=4, font=2, cex=.8)
text(6, 15.5, "SMD [95% CI]", pos=2, font=2, cex=.8)
text(c(-4.5), 15.5, c("Weight (%)"), font=2, cex=.8)

#Aggregation forest plot
agg_fp <- read_excel("/your/data/here")
res2 <- rma(ES, sei = se, data=agg_fp)
mlabfun <- function(text, x) {
  list(bquote(paste(.(text), 
                    "; ", I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    "df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
}
forest(res2, xlim=c(-16, 6), at=c(-1.5, 0, 1.5), ilab=cbind(agg_fp$Weight), ilab.xpos=c(-4.5),
       cex=0.75, ylim=c(-4, 17), order=alloc, rows=c(10:13, 2:6),
       psize=1, slab=agg_fp$StudyID)
op <- par(cex=0.75, font=2)
par(cex=0.75, font=4)
text(-16, c(7, 14), pos=4, c("3.2 No aggregation", "3.1 Aggregation"))
par(op)
res.r4 <- rma(ES, sei = se, subset=(alloc=="agg"), data=agg_fp)
res.a4 <- rma(ES, sei = se, subset=(alloc=="noagg"), data=agg_fp)
addpoly(res.r4, row=0.7, mlab=mlabfun("RE Model for No aggregation", res.r4))
addpoly(res.a4, row=8.7, mlab=mlabfun("RE Model for Aggregation", res.a4))
res2 <- rma(ES, sei = se, mods = ~ alloc, data=agg_fp)
text(-16, -2, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                            "df = ", .(res2$p - 1), ", ",
                                            .(fmtp(res2$QMp, digits=2, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
text(-16, 15.5, "Study", pos=4, font=2, cex=.8)
text(6, 15.5, "SMD [95% CI]", pos=2, font=2, cex=.8)
text(c(-4.5), 15.5, c("Weight (%)"), font=2, cex=.8)

