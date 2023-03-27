#-----------------------------------------------------
# Make fake plots to generate legend waterbands plots, then save legend as image
#-----------------------------------------------------
library(ggplot2)
library(ggpubr)
library(gridExtra)

full_data = data.frame(month = 1:5,
                       val = 5:9,
                       val2 = 4:8,
                       var = 
                         factor(c("Historic range", "Historic 95% range", "Historic 50% range",
                                  "Historic median", "WQ threshold"),
                                levels = c("Historic range", "Historic 95% range", "Historic 50% range",
                                           "Historic median", "WQ threshold")))

point_data = data.frame(month = c(1, 3),
                        val = c(5, 7),
                        var = as.factor(c("Current value", "Poor WQ value")),
                        type = 'point')

fake_bands <-
  ggplot(full_data, aes(x = month, y = val))+
  labs(y = NULL, x = NULL, title = NULL) + 
  geom_ribbon(aes(ymax = val, ymin = val2, fill = var))+
  geom_line(aes(color = var, linetype = var, linetype = var), size = 1)+
  scale_color_manual(values = 
                       c("Historic range" = "#E4F0F8", 
                         "Historic 95% range" = "#B8D8ED", 
                         "Historic 50% range" = "#7FB9DD", 
                         "Historic median" = "#1378b5", 
                         "WQ threshold" = "black"),
                     breaks = 
                       c("Historic range", 
                         "Historic 95% range", 
                         "Historic 50% range", 
                         "Historic median", 
                         "WQ threshold"),
                     name = NULL)+
  scale_linetype_manual(values =
                          c("Historic range" = 'solid', 
                            "Historic 95% range" = 'solid', 
                            "Historic 50% range" = 'solid', 
                            "Historic median" = 'solid', 
                            "WQ threshold" = 'dashed'),
                        breaks = 
                          c("Historic range", 
                            "Historic 95% range", 
                            "Historic 50% range", 
                            "Historic median", 
                            "WQ threshold"),
                        name = NULL)+
  scale_fill_manual(values = 
                      c("Historic range" = "#E4F0F8", 
                        "Historic 95% range" = "#B8D8ED", 
                        "Historic 50% range" = "#7FB9DD", 
                        "Historic median" = "white", 
                        "WQ threshold" = "white"), 
                    breaks = 
                      c("Historic range", 
                        "Historic 95% range", 
                        "Historic 50% range", 
                        "Historic median", 
                        "WQ threshold"),
                    name = NULL) +
  theme(legend.position = 'bottom',
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank(),
        legend.key.width = unit(1.1, 'cm'))

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
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = '#696969', fill = 'white', linewidth = 0.4),
        axis.line.x = element_line(color = "#696969", linewidth = 0.4),
        axis.line.y = element_line(color = "#696969", linewidth = 0.4),
        axis.ticks = element_line(color = "#696969", linewidth = 0.4),
        legend.key = element_blank()) 

leg_bands_pb <- as_ggplot(get_legend(fake_bands))
leg_points_pb <- as_ggplot(get_legend(fake_points))

leg <- grid.arrange(grobs = list(leg_bands_pb, leg_points_pb), 
                    nrow = 1, ncol = 3,
                    heights = c(0.5),
                    widths = c(23.5, 0.02, 4),
                    layout = c(1, NA, 2))

ggsave("./rmd/waterband_leg.svg", as_ggplot(leg), width = 12, height = 0.25)
#legimg2 <- magick::image_read_pdf("./rmd/waterband_leg.pdf", density = 600)

#ggsave("./rmd/waterband_leg.pdf", as_ggplot(leg), width = 12, height = 0.25)
#ggsave("./rmd/waterband_leg.png", as_ggplot(leg), width = 12, height = 0.3)
