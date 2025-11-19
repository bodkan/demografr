library(hexSticker)

sticker("drawing.png", package = "demografr",
        p_size=130, p_color = "black", p_y = 0.53, p_family = "sans", p_fontface = "bold.italic",
        h_fill = "white", h_color = "#4d4d4d", h_size = 3,
        s_x = 1, s_y = 1.22, s_width = 0.65, dpi = 2000,
        filename = "sticker.png", white_around_sticker = TRUE)

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
system("mv sticker.png man/figures/logo.png")
system("convert man/figures/logo.png  -resize 270x270 man/figures/logo.png")
# system("open man/figures/logo.png")
