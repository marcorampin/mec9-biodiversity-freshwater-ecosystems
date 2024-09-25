setwd("/home/marco/Documents/University/UniVie/2024S/MEC_9_Field_course")

library("ggplot2")
library("GGally")
library("dplyr")
library("ggpubr")
library("grid")

lake_data <- read.delim(file="Lunzer_untersee_data.csv", header=T, sep=",", skip=1)

o2_filtered <- lake_data[c(1:4)] %>% filter(!is.na(O2_abs) & !is.na(Depth))

ggplot(data = o2_filtered) +
  aes(x = O2_abs, y = Depth) +
  geom_point() +
  geom_path() +
  ylim(30, 0) +
  xlab(expression(paste("Absolute Oxygen concentration (mg L"^{-1}, ")"))) +
  ylab("Depth (m)")

ggplot(data = o2_filtered) +
  aes(x = O2_rel, y = Depth) +
  geom_point() +
  geom_path() +
  ylim(30, 0) +
  xlab("Relative Oxygen concentration (%)") +
  ylab("Depth (m)")

ggplot(data = o2_filtered) +
  aes(x = Temperature, y = Depth) +
  geom_point() +
  geom_path() +
  ylim(30, 0) +
  xlab("Temperature (°C)") +
  ylab("Depth (m)")


pH_filtered <- lake_data[c(1, 5)] %>% filter(!is.na(pH) & !is.na(Depth))

ggplot(data = pH_filtered) +
  aes(x = pH, y = Depth) +
  geom_point() +
  geom_path() +
  ylim(30, 0) +
  ylab("Depth (m)")

cond_filtered <- lake_data[c(1, 6)] %>% filter(!is.na(Conductivity) & !is.na(Depth))

ggplot(data = cond_filtered) +
  aes(x = Conductivity, y = Depth) +
  geom_point() +
  geom_path() +
  ylim(30, 0) +
  ylab("Depth (m)") +
  xlab(expression(paste("Conductivity (", mu, "S cm"^{-1}, ")")))


light_filtered <- lake_data[c(1, 7)] %>% filter(!is.na(Light_intensity) & !is.na(Depth))

ggplot(data = light_filtered) +
  aes(x = Light_intensity, y = Depth) +
  geom_point() +
  geom_path() +
  ylim(30, 0)


light_color_filtered <- lake_data[c(1, 7:10)] %>% filter(!is.na(Light_intensity) & !is.na(Depth))

ggplot(data = light_color_filtered, aes(y = Depth)) +
  aes(x = Light_intensity_red) +
  geom_point(aes(x = Light_intensity), color = "black") +
  geom_point(aes(x = Light_intensity_green), color = "#00BE67") +
  geom_point(aes(x = Light_intensity_red), color = "#F8766D") +
  geom_point(aes(x = Light_intensity_blue), color = "#00A9FF") +
  geom_path(aes(x = Light_intensity_green), color = "#00BE67") +
  geom_path(aes(x = Light_intensity_red), color = "#F8766D") +
  geom_path(aes(x = Light_intensity_blue), color = "#00A9FF") +
  geom_path(aes(x = Light_intensity), color = "black") +
  ylim(30, 0) +
  ylab("Depth (m)") +
  xlab(expression(paste("Light intensity (", mu, "mol m"^{-2}, " s"^{-1}, ")")))

attenuation_coeff_filtered <- lake_data[1:24, c(1, 12:15)] %>% filter(!is.na(light_attenuation) & !is.na(Depth))

ggplot(data = attenuation_coeff_filtered, aes(y = Depth)) +
  aes(x = Light_intensity_red) +
  geom_point(aes(x = light_attenuation), color = "black") +
  geom_point(aes(x = green_attenuation), color = "#00BE67") +
  geom_point(aes(x = red_attenuation), color = "#F8766D") +
  geom_point(aes(x = blue_attenuation), color = "#00A9FF") +
  geom_path(aes(x = green_attenuation), color = "#00BE67") +
  geom_path(aes(x = red_attenuation), color = "#F8766D") +
  geom_path(aes(x = blue_attenuation), color = "#00A9FF") +
  geom_path(aes(x = light_attenuation), color = "black") +
  ylim(30, 0) +
  ylab("Depth (m)") +
  xlab(expression(paste("Attenuation coefficient (m"^{-1},")")))


rel_light_filter <- lake_data[c(1, 21:22)] %>% filter(!is.na(relative_intensity) & !is.na(Depth))
ggplot(data = rel_light_filter) +
  aes(x = light_color, y = relative_intensity, color = light_color, group = Depth) +
  geom_point() +
  geom_line(aes(group = Depth), color = "darkgrey") +
  scale_color_manual(values = c("blue" = "#00A9FF", "green" = "#00BE67", "red" = "#F8766D")) +
  ylim(3, 100) +
  theme(legend.position = "none") +
  ylab("Relative light intensity (%)") +
  xlab("Filtered light")


secchi_filtered <- lake_data[c(1, 11, 20)] %>% filter(!is.na(euphotic_zone))

ggplot(data = secchi_filtered) +
  geom_boxplot(aes(x = "Secchi plate", y = Secchi_depth*2, fill="Secchi")) +
  geom_boxplot(aes(x = "Light attenuation", y = euphotic_zone, fill="Euphotic zone")) +
  ylab("Depth (m)") +
  xlab("Calculation method") +
  ylim(20, 10) +
  theme(legend.position = "none")


abundance_list <- read.delim(file="abundance.csv", header=T, sep=",")

ggplot(abundance_list, aes(x="", y=X, fill=Lunz.untersee)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
