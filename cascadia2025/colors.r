# color-blind friendly palettes

pal <- c("#000000","#004949","#bbc2c2","#ff6db6", "#ffb6db", "#490092","#1a5270","#b66dff","#508ca7","#8dc7dc", "#920000","#924900","#db6d00","#24ff24","#ffff6d")
 
 small_pal <- c("#8dc7dc","#508ca7","#1a5270", "#920000", "#555555", "#395333")

# ihp colors
log_breaks <- c(16, 18, 20, 22, 24)
linear_labels <- round(exp(log_breaks), -6) / 1000000
formatted_labels <- paste0("$", linear_labels, " million")
ihp_colors <- scale_fill_gradientn(colors = c("#E8F5E9", "#81C784", "#4c825d", "#395333"), name = "",
labels = formatted_labels)

# disaster colors
disasters_colors <- scale_fill_gradientn(colors = c("#d6e5eb", "#8dc7dc","#508ca7","#1a5270"), name = "")

hex_codes <- c("#134130", "#4c825d", "#8cae9e", "#8dc7dc", "#508ca7", "#1a5270", "#0e2a4d")
