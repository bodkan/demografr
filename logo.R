library(hexSticker)

sticker("logo.png", package = "demografr",
        p_size=130, p_color = "black", p_y = 0.52, p_family = "sans", p_fontface = "bold",
        h_fill = "white", h_color = "black", h_size = 3,
        s_x = 1, s_y = 1.22, s_width = 0.65, dpi = 2000,
        filename = "sticker.png", white_around_sticker = TRUE)
system("mv sticker.png man/figures/logo.png")
system("convert man/figures/logo.png  -resize 300x300 man/figures/logo.png")
# system("open man/figures/logo.png")
