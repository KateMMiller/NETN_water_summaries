#-----------------------------------------------------
# Make fake plots to generate legend waterbands plots and waterboxplots
#-----------------------------------------------------
library(ggplot2)
library(ggpubr)
library(gridExtra)

full_data = data.frame(month = 1:5,
                       val = 5:9,
                       val2 = 4:8,
                       var = 
                         factor(c("Historic range", "Hist. 95% range", "Hist. 50% range",
                                  "Hist. median", "WQ threshold"),
                                levels = c("Historic range", "Hist. 95% range", "Hist. 50% range",
                                           "Hist. median", "WQ threshold")),
                       grp = c(rep('g1', 5)))

point_data = data.frame(month = c(1, 3),
                        val = c(5, 7),
                        var = as.factor(c("Current value", "Poor WQ value")),
                        type = 'point', 
                        grp = as.factor(c("Current value", "Poor WQ value")),
                        grp2 = c(rep("Historic outlier", 2)))

fake_bands <-
  ggplot(full_data, aes(x = month, y = val))+
  labs(y = NULL, x = NULL, title = NULL) + 
  geom_ribbon(aes(ymax = val, ymin = val2, fill = var))+
  geom_line(aes(color = var, linetype = var, linetype = var), size = 1)+
  scale_color_manual(values = 
                       c("Historic range" = "#E4F0F8", 
                         "Hist. 95% range" = "#B8D8ED", 
                         "Hist. 50% range" = "#7FB9DD", 
                         "Hist. median" = "#1378b5", 
                         "WQ threshold" = "black"),
                     breaks = 
                       c("Historic range", 
                         "Hist. 95% range", 
                         "Hist. 50% range", 
                         "Hist. median", 
                         "WQ threshold"),
                     name = NULL)+
  scale_linetype_manual(values =
                          c("Historic range" = 'solid', 
                            "Hist. 95% range" = 'solid', 
                            "Hist. 50% range" = 'solid', 
                            "Hist. median" = 'solid', 
                            "WQ threshold" = 'dashed'),
                        breaks = 
                          c("Historic range", 
                            "Hist. 95% range", 
                            "Hist. 50% range", 
                            "Hist. median", 
                            "WQ threshold"),
                        name = NULL)+
  scale_fill_manual(values = 
                      c("Historic range" = "#E4F0F8", 
                        "Hist. 95% range" = "#B8D8ED", 
                        "Hist. 50% range" = "#7FB9DD", 
                        "Hist. median" = "white", 
                        "WQ threshold" = "white"), 
                    breaks = 
                      c("Historic range", 
                        "Hist. 95% range", 
                        "Hist. 50% range", 
                        "Hist. median", 
                        "WQ threshold"),
                    name = NULL) +
  theme(legend.position = 'bottom',
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank(),
        legend.key.width = unit(0.8, 'cm'))

fake_points <-
  ggplot(point_data, aes(x = month, y = val, color = var))+
  geom_point()+
  labs(y = NULL, x = NULL, title = NULL) + 
  scale_color_manual(values = 
                       c("Current value" = 'black', 
                         "Poor WQ value" = "orange"),
                     breaks = 
                       c("Current value", 
                         "Poor WQ value"),
                     name = NULL)+
  theme(legend.position = 'bottom',
        axis.title.y = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank()) 

leg_bands_pb <- as_ggplot(get_legend(fake_bands))
leg_points_pb <- as_ggplot(get_legend(fake_points))

legg <- as_ggplot(
  grid.arrange(grobs = list(leg_bands_pb, leg_points_pb),
               nrow = 1, ncol = 2,
               heights = c(0.25),
               widths = c(28, 12),
               layout = c(1, 2)))

#legg <- as_ggplot(leg)

#ggsave("./rmd/waterband_leg.svg", as_ggplot(leg), width = 12, height = 0.25)
#legimg2 <- magick::image_read_pdf("./rmd/waterband_leg.pdf", density = 600)

#ggsave("./rmd/waterband_leg.pdf", as_ggplot(leg), width = 12, height = 0.25)
#ggsave("./rmd/waterband_leg.png", as_ggplot(leg), width = 12, height = 0.3)


fake_boxplots <-
ggplot(full_data, aes(x = as.factor(grp), y = val, fill = grp, color = grp))+#, group = grp))+
  labs(y = NULL, x = NULL, title = NULL) +
  geom_boxplot(alpha = 0.85, lwd = 0.75) +
  scale_fill_manual(name = NULL, labels = c("Historic range"),
                    values = c("#1378b5"))+
  scale_color_manual(name = NULL, labels = c("Historic range"),
                     values = c("#1378b5"))+
  theme(legend.position = 'bottom',
        legend.key = element_blank())


fake_lines <-
  ggplot(full_data, aes(x = month, y = val, color = grp, fill = grp, linetype = grp)) +
  geom_line(aes(linetype = grp), lty = 'dashed', size = 1) +
  scale_color_manual(name = NULL, values = "black",
                     labels = "WQ threshold", breaks = "g1") +
  scale_linetype_manual(name = NULL, values = "dashed",
                     labels = "WQ threshold", breaks = "g1") +
  theme(legend.position = 'bottom',
        legend.key = element_blank(),
        legend.key.width = unit(1, 'cm'))


fake_outs <-
  ggplot(point_data, aes(x = month, y = val, color = grp2, shape = grp2)) +
  geom_point(aes(shape = grp2, color = grp2), shape = 8, size = 2) +
  labs(y = NULL, x = NULL, title = NULL) + 
  scale_color_manual(breaks = "Historic outlier",
                     values = "#1378b5",
                     labels = "Historic outlier",
                     name = NULL)+
  theme(legend.position = 'bottom',
        legend.key = element_blank())

leg_box <- as_ggplot(get_legend(fake_boxplots))
leg_lines <- as_ggplot(get_legend(fake_lines))
leg_points <- as_ggplot(get_legend(fake_points))
leg_outs <- as_ggplot(get_legend(fake_outs))

legbox <- 
  as_ggplot(
  grid.arrange(grobs = list(leg_box, leg_lines, leg_points, leg_outs),
               nrow = 1, ncol = 6,
               heights = c(0.25),
               widths = c(1, 1, 1.8, 0.8, 0.6, 1),
               layout = c(NA, 1, 2, 3, 4, NA)))

