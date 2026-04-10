plotFreqs = function(freqs, marker = NULL) {
  df = data.frame(allele = names(freqs), freq = unname(freqs))
  df$anum = suppressWarnings(as.numeric(df$allele))
  df = df[!is.na(df$anum), , drop = FALSE]

  df$grp = floor(df$anum)
  df = df[order(df$grp, df$anum), ]
  df$class = ifelse(df$anum == df$grp, "whole", sub("^[0-9]+", "", df$allele))

  # x-axis spacing: groups separated by 1; within groups sep 0.4
  has_whole = ave(df$anum == df$grp, df$grp, FUN = any)
  d = ifelse(c(TRUE, diff(df$grp) != 0), 1 + 0.25 * !has_whole, 0.40)
  d[1] = d[1] - 1
  df$x = 1 + cumsum(d)

  xlab = aggregate(x ~ grp, df, min)
  xlab$x = xlab$x - 0.25*!tapply(df$anum == df$grp, df$grp, any)[as.character(xlab$grp)]
  xlabsize = c(5, 4.5, 4, 3.5)[min(4, 1 + length(unique(df$grp)) %/% 10)]

  ggplot2::ggplot(df, ggplot2::aes(x, freq)) +
    ggplot2::geom_linerange(data = xlab,
      ggplot2::aes(x = x, ymin = 0, ymax = 0.001),
      linewidth = 0.5, colour = "grey55", inherit.aes = FALSE,
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = 0, ymax = freq),
      linewidth = 0.5,
      colour = "grey55"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = class),
      shape = 21,
      size = 4.5,
      stroke = 0.5,
      colour = "black"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.02, 0.02)),
      breaks = xlab$x,
      labels = xlab$grp
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0.1, 0.04))
    ) +
    ggplot2::geom_text(data = xlab, ggplot2::aes(x = x, y = 0, label = grp),
                       vjust = 2, inherit.aes = FALSE, size = xlabsize) +
    ggplot2::scale_fill_manual(
      values = c("whole" = "beige",
                 ".1" = "lightblue", ".2" = "orange",
                 ".3" = "lightgreen", ".4" = "pink"),
      breaks = c(".1", ".2", ".3", ".4"),
      labels = paste0("\u2024", 1:4),
      name = NULL
    ) +
    ggplot2::labs(title = marker, x = NULL, y = "Frequency") +
    ggplot2::theme_light(base_size = 15) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = "grey40", linewidth = 0.4),
      panel.border = ggplot2::element_rect(colour = "grey55", fill = NA, linewidth = 0.6),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = c(0.99, 0.99),
      #legend.text = ggplot2::element_text(face = "bold"),
      legend.justification = c(1, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = "grey70"),
      legend.key.width = grid::unit(12, "pt"),
    )
}

# plotFreqs(forrel::NorwegianFrequencies$D12S391)
