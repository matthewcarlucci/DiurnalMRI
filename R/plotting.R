##### Plotting #####
# Mostly shortcuts/wrappers for labels/colors and plot themes

dx2lab <- function(x) {
  return(ifelse(as.character(x) == "1", "BPD", "CNTRL"))
}

# mutually exclusive labels for significance
pqlabs <- function(p = NULL, q = NULL) {
  if (!is.null(p) & is.null(q)) {
    lab <- rep("p>0.05", length(p))
    lab[p < 0.05] <- "p<0.05"
    levs <- c("p>0.05", "p<0.05")
  } else if (is.null(p) & !is.null(q)) {
    lab <- rep("q>0.05", length(q))
    lab[q < 0.05] <- "q<0.05"
    levs <- c("q>0.05", "q<0.05")
  } else if (!is.null(p) & !is.null(q)) {
    stopifnot(length(p) == length(q))
    lab <- rep("p>0.05", length(p))
    lab[p < 0.05] <- "p<0.05"
    lab[q < 0.05] <- "q<0.05"
    levs <- c("p>0.05", "p<0.05", "q<0.05")
  }
  return(factor(lab, levels = levs))
}


# Modified from https://stackoverflow.com/questions/10762287/how-can-i-format-axis-labels-with-exponents-with-ggplot2-and-scales
my_val_formatter <- function(x, doparse = T, mf = T, # math formatting?
                             ...) {
  ifelse(x > 1e4 | (x > 0 & x < 1e-3) | (x < 0 & x > -1e-3),
    {
      txt1 <- scales::scientific_format(digits = 2)(x)
      if (mf) {
        txt <- gsub("e\\+*", " %*% 10^", txt1)
      } else {
        return(txt1)
      }
      if (doparse) {
        return(parse(text = txt))
      } else {
        return(txt)
      }
    },
    formatC(x, format = "fg", digits = 2, flag = "#", big.mark = ",")
  )
}

# Formatter for tables
myfmt <- function(x) formatC(x, digits = 2)
myfmt2 <- function(x) my_val_formatter(x, mf = F)

# Common text reductions especially useful for multipanel figures
theme_condense <- function() {
  list(
    theme(strip.background = element_rect(color = "white", fill = "white")),
    theme(strip.text = element_text(
      size = 7, color = "black",
      margin = margin(b = 0, t = 0)
    )),
    theme(axis.text = element_text(size = 7))
  )
}

theme_condense_legend <- function() {
  list(
    guides(shape = guide_legend(override.aes = list(size = 0.3))),
    guides(color = guide_legend(override.aes = list(size = 0.3))),
    guides(fill = guide_legend(override.aes = list(size = 0.3))),
    theme(
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.3, "cm")
    )
  )
}

# Template for circadian rose diagrams
coord_circpolar <- function(...) {
  list(
    coord_polar(...),
    scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 3)),
    scale_y_continuous(limits = c(-1, NA)),
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank()
    ),
    geom_hline(yintercept = 0, color = "grey")
  )
}

## Colors - set some global color schemes

colors <- list(
  gcos_sig = "blue",
  ssc = "brown",
  scc_sig = "red",
  nonsig = "grey",
  raw_data = "grey20",
  dx1 = "purple",
  dx01 = "black",
  fishers = "darkred"
)
colors$dx0 <- "darkorange"

## ggplot shortcuts

# Not working and wasnt able to make behave as desired
# geom_smooth_cosinor <- function(per=24, ...) geom_smooth(method="lm",formula=y ~ sinpi(x/per/2) + cospi(x/per/2),...)
geom_smooth_cosinor <- function(...) geom_smooth(method = "lm", formula = y ~ sinpi(x / 12) + cospi(x / 12), ...)
stat_smooth_cosinor <- function(...) stat_smooth(geom = "line", method = "lm", formula = y ~ sinpi(x / 12) + cospi(x / 12), ...)

scale_x_circ <- function() scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 6))

# rot: hours (integer) to rotate by to find a good scheme
scale_fill_clock <- function(rot = 0, ...) scale_fill_gradientn(colors = rainbow(24)[((1:24) - 1 + rot) %% 24 + 1], limits = c(0, 24), ...)
scale_color_clock <- function(rot = 0, ...) scale_color_gradientn(colors = rainbow(24)[((1:24) - 1 + rot) %% 24 + 1], limits = c(0, 24), ...)

scale_color_fgbg <- function(fgcol = "red", ...) scale_color_manual(values = c("TRUE" = fgcol, "FALSE" = "grey20", ...))
scale_alpha_fgbg <- function(alpha_bg = 0.3, ...) scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = alpha_bg, ...))
scale_fill_fgbg <- function(fgcol = "red", ...) scale_fill_manual(values = c("TRUE" = fgcol, "FALSE" = "grey20", ...))

# Quick access to groupwise colorings
scale_color_dx <- function() {
  scale_color_manual(values = c("1" = colors$dx1, "0" = colors$dx0))
}
scale_color_dxlab <- function(...) {
  scale_color_manual(values = c(
    "BPD" = colors$dx1,
    "CNTRL" = colors$dx0, ...
  ))
}
scale_fill_dx <- function() {
  scale_fill_manual(values = c("1" = colors$dx1, "0" = colors$dx0))
}
scale_fill_dxlab <- function() {
  scale_fill_manual(values = c(
    "BPD" = colors$dx1,
    "CNTRL" = colors$dx0
  ))
}


# Defining "dark" times
night_rect <- function() {
  annotate(
    geom = "rect", ymin = -Inf, ymax = Inf, xmin = 24,
    xmax = 32, alpha = 0.8, fill = "grey", color = NA
  )
}
night_recty <- function() {
  annotate(
    geom = "rect", xmin = -Inf, xmax = Inf, ymin = 24,
    ymax = 32, alpha = 0.8, fill = "grey", color = NA
  )
}
night_rect_early <- function() {
  annotate(
    geom = "rect", ymin = -Inf, ymax = Inf, xmin = 0,
    xmax = 8, alpha = 0.8, fill = "grey", color = NA
  )
}
night_recty_early <- function() {
  annotate(
    geom = "rect", xmin = -Inf, xmax = Inf, ymin = 0,
    ymax = 8, alpha = 0.8, fill = "grey", color = NA
  )
}
