# I reworked this because of broken rendering of the hexagonal outline:
# https://github.com/GuangchuangYu/hexSticker/issues/155#issuecomment-3351691700
library(hexSticker)
library(ggplot2)

p <- sticker("drawing.png", package = "demografr",
        p_size = 65, p_color = "black", p_y = 0.5, p_family = "sans", p_fontface = "bold.italic",
        h_fill = "white", h_color = "black", h_size = 10,
        s_x = 1, s_y = 1.22, s_width = 0.65, dpi = 2000,
        white_around_sticker = TRUE) +
  theme(plot.margin = margin(l= -.2, unit = "lines"))
ggsave(filename = "sticker.png", plot = p, bg = "transparent")
# system("open sticker.png")

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
system("mv sticker.png man/figures/logo.png")
system("convert man/figures/logo.png  -resize 270x270 man/figures/logo.png")
system("open man/figures/logo.png")
