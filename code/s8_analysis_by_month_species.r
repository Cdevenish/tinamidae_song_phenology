#### Final models ####

library(dplyr)
library(mgcv)
library(ggplot2)
library(gratia)
library(patchwork)

load("./data/ebd_x_mes_species.rdata") ## ebd_x_mes

head(ebd_x_mes)
str(ebd_x_mes)
# covnert categorical covariates to factors
ebd_x_mes$scientific_name <- factor(ebd_x_mes$scientific_name)
ebd_x_mes$day_length <- as.numeric(ebd_x_mes$day_length)

# scale numeric covariates
ebd_x_mes$mean_prec <- scale(ebd_x_mes$mean_prec)
ebd_x_mes$day_length <- scale(ebd_x_mes$day_length)


head(ebd_x_mes)


# try poisson family

# use cyclical smooth for months
knots <- list(month_no = c(0.5, 12.5))

gam1_pois <- gam(n ~ s(mean_prec, bs = "ts") +
                   s(day_length, bs = "ts") +  
                   s(scientific_name, bs = "re") + 
                   s(k5, latBand, bs = "re")+
                   s(month_no, bs = "cc", k = 12),
                   data = ebd_x_mes, knots = knots, family = poisson)

gratia::appraise(gam1_pois) # not great fit
AIC(gam1_pois)
summary(gam1_pois)


# try neg binomial
gam1_nb <- gam(n ~ s(mean_prec, bs = "ts") + 
                 s(day_length, bs = "ts") +  
                 s(month_no, bs = "cc", k = 12) +
                 s(scientific_name, bs = "re") + 
                 s(k5, latBand, bs = "re"),
               data = ebd_x_mes, knots = knots, family = nb)
#
AIC(gam1_nb) # 10962.6
gratia::appraise(gam1_nb) # looks better
gam.check(gam1_nb)
summary(gam1_nb)
gratia::draw(gam1_nb)


# Family: Negative Binomial(1.059) 
# Link function: log 
# 
# Formula:
#   n ~ s(mean_prec, bs = "ts") + s(day_length, bs = "ts") + s(scientific_name, 
#                                                              bs = "re") + s(k5, latBand, bs = "re") + s(month_no, bs = "cc", 
#                                                                                                         k = 12)
# 
# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    2.224      0.346   6.429 1.29e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
# s(mean_prec)        2.983      9 1075.7 3.31e-05 ***
#   s(day_length)       6.743      9 2024.2 4.59e-06 ***
#   s(scientific_name) 14.497     15 2485.9  < 2e-16 ***
#   s(k5,latBand)      16.644     17 5230.1  < 2e-16 ***
#   s(month_no)         5.881     10   51.3  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.412   Deviance explained = 57.6%
# -REML = 5546.6  Scale est. = 1         n = 1443


## table
tab_exp <- summary(gam1_nb)$s.table
write.csv(tab_exp, file = "./plots/table_mod1.csv")




## Plots

## Basic plots of 2 covariates
gratia::draw(gam1_nb, select = 1:3, scales = "fixed")



# By prec and latBand
gam_by_pR_lB <- gam(n ~ s(mean_prec, bs = "ts", by = k5) +
                   s(day_length, bs = "ts", by = latBand) +
                   s(month_no, bs = "cc", k = 12) + 
                   k5 + latBand +
                   s(scientific_name, bs = "re"),
                   data = ebd_x_mes, knots = knots, family = nb)

gratia::appraise(gam_by_pR_lB)
summary(gam_by_pR_lB)
gratia::draw(gam_by_pR_lB)


# Family: Negative Binomial(0.875) 
# Link function: log 
# 
# Formula:
#   n ~ s(mean_prec, bs = "ts", by = k5) + s(day_length, bs = "ts", 
#                                            by = latBand) + s(month_no, bs = "cc", k = 12) + k5 + latBand + 
#   s(scientific_name, bs = "re")
# 
# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)      3.55206    0.25285  14.048  < 2e-16 ***
#   k52             -1.06886    0.15309  -6.982 2.91e-12 ***
#   k53             -1.84783    0.13209 -13.989  < 2e-16 ***
#   k54             -0.54624    0.11845  -4.612 4.00e-06 ***
#   k55             -0.99255    0.11293  -8.789  < 2e-16 ***
#   latBand-15 to 0 -0.46347    0.13126  -3.531 0.000414 ***
#   latBand0 to 15  -0.02373    0.29578  -0.080 0.936056    
#   latBand15 to 30  1.03479    0.17374   5.956 2.59e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                                           edf Ref.df  Chi.sq  p-value    
#   s(mean_prec):k51                 0.857062      9   6.150   0.0137 *  
#   s(mean_prec):k52                 6.076337      9  69.027  < 2e-16 ***
#   s(mean_prec):k53                 0.002116      8   0.001   0.4353    
#   s(mean_prec):k54                 0.703279      9   2.342   0.0803 .  
#   s(mean_prec):k55                 0.745943      9   2.998   0.0506 .  
#   s(day_length):latBand-30 to -15  2.904896      9  44.735 1.48e-06 ***
#   s(day_length):latBand-15 to 0    3.462936      9  56.506 1.04e-07 ***
#   s(day_length):latBand0 to 15     5.205162      9 154.275  < 2e-16 ***
#   s(day_length):latBand15 to 30    2.106019      9  18.332 7.85e-05 ***
#   s(month_no)                      6.481845     10  71.288  < 2e-16 ***
#   s(scientific_name)              14.538212     15 525.372  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#   R-sq.(adj) =  0.214   Deviance explained = 48.4%
#   -REML = 5695.5  Scale est. = 1         n = 1443




## Tidy up plots using data directly - mainly to limit predictions to within observed data on x axis
gd2 <- gratia::draw(gam_by_pR_lB, select = 1:10)
str(gd2)

# last plot
str(gd2$data)
unique(gd2$data$smooth)

# rest of plots in $patches$plots
str(gd2$patches, 1)
str(gd2$patches$plots, 1)
str(gd2$patches$plots[[1]], 1)

# tmp2 <- gd2$patches$plots[[1]]$data
# tmp1 <- ggplot2::ggplot_build(gd2$patches$plots[[1]])
# head(tmp2)
# str(tmp1$data, 1)

## all data:
pltData <- lapply(gd2$patches$plots, function(x) ggplot_build(x)$data)
str(pltData, 1)
# add last plot
pltData <- c(pltData, list(ggplot_build(gd2)$data))

str(pltData, 1)
str(pltData, 2)
## order is ribbon, line, rug


# Do each group separately, add axis labels, and reconvert mean precipitation and day length to original scale
attributes(ebd_x_mes$mean_prec)

#### Mean prec
plts_p1 <- lapply(pltData[1:5], function(x){
  
  df1 <- x[[1]] # ribbon
  df2 <- x[[2]] # line
  df3 <- x[[3]] # rug
  
  df1$x <- (df1$x * attr(ebd_x_mes$mean_prec, "scaled:scale")) + attr(ebd_x_mes$mean_prec, "scaled:center")
  df2$x <- (df2$x * attr(ebd_x_mes$mean_prec, "scaled:scale")) + attr(ebd_x_mes$mean_prec, "scaled:center")
  df3$x <- (df3$x * attr(ebd_x_mes$mean_prec, "scaled:scale")) + attr(ebd_x_mes$mean_prec, "scaled:center")
  
  ggplot()+
    geom_line(data= df2, aes(x = x, y = y))+
    geom_ribbon(data = df1, aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.2)+
    geom_rug(data = df3, aes(x = x))+
    xlim(range(df3$x))+
    #ylim(range(df1$ymin[df1$x > min(df3$x) & df1$x < max(df3$x)],
    #            df1$ymax[df1$x > min(df3$x) & df1$x < max(df3$x)]))
    ylim(-3.2,1.2)+ ## add specific ylims for each group
    xlab("")+
    ylab("")
    #xlab("Mean monthly precipitation (mm)")+
    #ylab("Partial effect")
  
    # return data - alternative
    # list(df1, df2, df3)
  
  })

str(plts_p1, 2)

plts_p1[2]


library(patchwork)

p1 <- wrap_plots(plts_p1, ncol = 1, nrow = 5)+
  plot_annotation(tag_levels = "a", tag_suffix = ".")
p1


## Day length
attributes(ebd_x_mes$day_length)

plts_p2 <- lapply(pltData[6:9], function(x){
  
  df1 <- x[[1]] # ribbon
  df2 <- x[[2]] # line
  df3 <- x[[3]] # rug
  
  df1$x <- (df1$x * attr(ebd_x_mes$day_length, "scaled:scale")) + attr(ebd_x_mes$day_length, "scaled:center")
  df2$x <- (df2$x * attr(ebd_x_mes$day_length, "scaled:scale")) + attr(ebd_x_mes$day_length, "scaled:center")
  df3$x <- (df3$x * attr(ebd_x_mes$day_length, "scaled:scale")) + attr(ebd_x_mes$day_length, "scaled:center")
  
  ggplot()+
    geom_line(data= df2, aes(x = x, y = y))+
    geom_ribbon(data = df1, aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.2)+
    geom_rug(data = df3, aes(x = x))+
    xlim(range(df3$x))+
    # ylim(range(df1$ymin[df1$x > min(df3$x) & df1$x < max(df3$x)],
    #            df1$ymax[df1$x > min(df3$x) & df1$x < max(df3$x)]))
    ylim(-2.2,2.2)+ ## add specific ylims for each group
    xlab("Day length (hours)")+
    ylab("Partial effect")
  
})

p2 <- wrap_plots(plts_p2, ncol = 2, nrow = 2)+
  plot_annotation(tag_levels = "a", tag_suffix = ".")
p2


## Check
gratia::draw(gam_by_pR_lB, select = 1:5, scales = "fixed")
p1
gratia::draw(gam_by_lB, select = 5:8)
p2

# Save plots
ggsave(plot = p1, filename = "./plots/Fig3_prec_by_latBand.png", width = 100, height = 300, units = "mm", dpi = 300)
ggsave(plot = p2, filename = "./plots/Fig2_dayLength_by_latBand.png")


## add rain and daylength to plots

load("./data/precip_chk.rdata") ## pts, chk_pts, chck_pts_lng
head(chck_pts_lng)
table(subset(chck_pts_lng, kn == 5)$k)

p1_1 <- lapply(split(subset(chck_pts_lng, kn == 5), subset(chck_pts_lng, kn == 5)$k), function(p){
  
  ggplot(p, aes(x = month, y = prec))+
    geom_boxplot()+
    ylab("")+
    xlab("")+
    #ylab("Monthly precipiation (mm)")+
    #xlab("Month")+
    theme(axis.text.x = element_text(angle = 90))
}
)
  
  
p1_1[[5]] <- p1_1[[5]] + labs(x = "Month")
plts_p1[[5]] <- plts_p1[[5]] + labs(x = "Mean monthly precipitation (mm)")


design = "
#AF
KBG
KCH
KDI
#EJ
"

dummy <- ggplot(data.frame(x = 1), aes(x = x))+
  geom_histogram()+
  ylab("Mean precipitation (mm)")
dummy

y_axis <- cowplot::get_plot_component(dummy, "ylab-l")

plots <- list(p1_1[[1]],p1_1[[2]], p1_1[[3]], p1_1[[4]], p1_1[[5]], 
           plts_p1[[1]], plts_p1[[2]], plts_p1[[3]], plts_p1[[4]], plts_p1[[5]],
           y_axis)
str(plots,1)

p4 <- wrap_plots(plots, ncol = 2, nrow = 5)+
  plot_layout(widths = c(1, 50, 50), design = design)+
  #plot_annotation(tag_levels = "a", tag_suffix = ".")
  plot_annotation(tag_levels = list(letters[1:5]))

p4

ggsave(plot = p4, filename = "./plots/Fig3_prec_by_precR.png", width = 200, height = 300, units = "mm", dpi = 300)




## Do other plot to match style
gratia::draw(gam1_nb_cycl, select = c(1:2,5))
gd1 <- gratia::draw(gam1_nb_cycl, select = c(1:2,5))

# extract data and rescale
plt_1_Data <- c(lapply(gd1$patches$plots, function(x) { # overkill with this first lapply, just one plot... 
  lapply(ggplot_build(x)$data, function(df){
    #df$x <- (df$x * attr(ebd_x_mes$mean_prec, "scaled:scale")) + attr(ebd_x_mes$mean_prec, "scaled:center")
    df
  })
  }), 
  # add last plot
  list(lapply(ggplot_build(gd1)$data, function(df){
    #df$x <- (df$x * attr(ebd_x_mes$day_length, "scaled:scale")) + attr(ebd_x_mes$day_length, "scaled:center")
    df
    })))

str(plt_1_Data, 1)
str(plt_1_Data, 2)

str(plt_1_Data[[i]], 2)

## rescale

plt_1_df <- list()

for(i in seq_along(plt_1_Data)){
  
  if(i == 1){
    plt_1_df[[i]] <- lapply(plt_1_Data[[i]], function(df){
    df$x <- (df$x * attr(ebd_x_mes$mean_prec, "scaled:scale")) + attr(ebd_x_mes$mean_prec, "scaled:center")
    df
  })
  }
  
  if(i == 2){
    plt_1_df[[i]] <- lapply(plt_1_Data[[i]], function(df){
      df$x <- (df$x * attr(ebd_x_mes$day_length, "scaled:scale")) + attr(ebd_x_mes$day_length, "scaled:center")
      df
    })
  }
  
  if(i == 3) plt_1_df[[i]] <- plt_1_Data[[i]]
  
  }

str(plt_1_df, 2)
str(plt_1_df[[3]][[3]], 1)
plot(x = plt_1_df[[3]][[3]]$x, y = rep(1, 1443))



plts1 <- lapply(plt_1_df, function(x){
  
  df1 <- x[[1]] # ribbon
  df2 <- x[[2]] # line
  df3 <- x[[3]] # rug
  
  ggplot()+
    geom_line(data= df2, aes(x = x, y = y))+
    geom_ribbon(data = df1, aes(x = x, ymin = ymin, ymax = ymax), alpha = 0.2)+
    geom_rug(data = df3, aes(x = x))+
    xlim(range(df3$x))+
    ylim(-2, 1.5)+
    ylab("Partial effect")
    
})

plts1

## add specific xlabs for each
plts_p0 <- list(plts1[[1]]+xlab("Mean monthly precipitation (mm)"), 
                plts1[[2]]+xlab("Day length (hours)"),
                plts1[[3]]+xlab("Month")+scale_x_continuous(breaks = 1:12, labels = 1:12, minor_breaks = seq(0,12,2)))

p0 <- wrap_plots(plts_p0, ncol = 3, nrow = 1)+
  plot_annotation(tag_levels = "a", tag_suffix = ".")
p0

ggplot2::ggsave(plot = p0, "./plots/Fig1_modelo_response_precipitacion_day_length.png", dpi = 600)



shell.exec(file.path(getwd(), "plots"))

##################

### 4 x 2 arrangement

p3 <- wrap_plots(c(plts_p1, plts_p2), ncol = 2, nrow = 4, byrow = FALSE)+
  plot_annotation(tag_levels = "a", tag_suffix = ".")

p3

ggsave(plot = p3, filename = "./plots/Fig2_day_length_prec_by_latBand.png", 
       width = 250, height = 350, units = "mm", dpi = 300)
