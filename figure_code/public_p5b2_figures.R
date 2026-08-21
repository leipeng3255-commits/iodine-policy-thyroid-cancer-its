#!/usr/bin/env Rscript
# P5B.2 publication exports.  This script uses only locked P3C/P4F display data.
# It changes Figure 1 and Figure 5 wording/architecture, never analytical values.

options(stringsAsFactors = FALSE, scipen = 999)
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1]))))
setwd(root)
stopifnot(requireNamespace("ggplot2", quietly = TRUE))
library(ggplot2)

out <- file.path(root, "figures")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

save_art <- function(draw_fun, base, width, height) {
  pdf_file <- paste0(base, ".pdf")
  png_file <- paste0(base, ".png")
  tiff_file <- paste0(base, ".tiff")
  pdf(pdf_file, width = width, height = height, family = "Helvetica", useDingbats = FALSE, bg = "white")
  draw_fun()
  dev.off()
  png_backend <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else if (capabilities("cairo")) "cairo" else getOption("bitmapType")
  png(png_file, width = width * 300, height = height * 300, res = 300, type = png_backend, bg = "white")
  draw_fun()
  dev.off()
  converter <- Sys.which("pdftocairo")
  if (!nzchar(converter)) stop("pdftocairo is required for deterministic TIFF export")
  prefix <- tempfile("p5b2_figure_")
  status <- system2(converter, c("-tiff", "-r", "600", "-singlefile", "-tiffcompression", "lzw", pdf_file, prefix))
  if (status != 0 || !file.exists(paste0(prefix, ".tif"))) stop("TIFF conversion failed")
  if (!file.copy(paste0(prefix, ".tif"), tiff_file, overwrite = TRUE)) stop("TIFF copy failed")
}

fig1 <- read.csv("Iodine_PublicP4F_Figure1_SourceData_DisplayReady.csv", check.names = FALSE)
fig1$role <- c(Australia = "Primary setting", `New Zealand` = "Primary setting", Croatia = "Supporting setting")[fig1$country]
write.csv(fig1, file.path(out, "Figure_1_SourceData.csv"), row.names = FALSE, na = "")

draw_figure1 <- function() {
  par(mfrow = c(3, 1), mar = c(2.2, 8.7, 2.2, 2.0), oma = c(4.2, 0.5, 3.0, 0.5), family = "sans", xaxs = "i", bg = "white")
  xlim <- c(1980, 2025)
  track_cols <- c(POLICY = "#D55E00", UIC = "#009E73", CANCER = "#0072B2")
  for (i in seq_len(nrow(fig1))) {
    s <- fig1[i, ]
    plot(NA, xlim = xlim, ylim = c(0.55, 3.45), xlab = "", ylab = "", axes = FALSE, bty = "n")
    abline(v = seq(1980, 2025, 5), col = "#ECECEC", lwd = 0.7)
    axis(2, at = c(3, 2, 1), labels = c("CANCER REGISTRY", "UIC CONTEXT", "POLICY"), las = 1, tick = FALSE, line = -0.6, cex.axis = 0.78, col.axis = "#333333")
    mtext(paste0(s$country, " (", s$role, ")"), side = 3, adj = 0, font = 2, cex = 1.10, line = 0.35, col = "#111111")
    segments(s$policy_year, 1, s$policy_year, 3.25, col = adjustcolor(track_cols["POLICY"], alpha.f = 0.35), lty = 2, lwd = 1.3)
    points(s$policy_year, 1, pch = 15, cex = 1.15, col = track_cols["POLICY"])
    text(s$policy_year + 0.45, 1, sprintf("%d  %s", s$policy_year, s$policy_description), adj = 0, cex = 0.70, col = "#333333")
    if (!is.na(s$pre_UIC_year)) {
      arrows(s$pre_UIC_year, 2, s$post_UIC_year, 2, length = 0.08, lwd = 2, col = track_cols["UIC"])
      points(c(s$pre_UIC_year, s$post_UIC_year), c(2, 2), pch = 16, cex = 1, col = track_cols["UIC"])
      text(mean(c(s$pre_UIC_year, s$post_UIC_year)), 2.27, paste0(s$pre_policy_UIC_summary_ug_L, " to ", s$post_policy_UIC_summary_ug_L, " ug/L"), cex = 0.74, col = track_cols["UIC"])
    } else {
      points(s$post_UIC_year, 2, pch = 16, cex = 1, col = track_cols["UIC"])
      text(s$post_UIC_year, 2.27, "No comparable pre-policy national UIC; post-policy 140 ug/L", cex = 0.70, col = track_cols["UIC"])
    }
    segments(s$cancer_followup_start, 3, s$cancer_followup_end, 3, lwd = 5, col = adjustcolor(track_cols["CANCER"], alpha.f = 0.72), lend = 1)
    points(s$primary_latency_break, 3, pch = 18, cex = 1.35, col = "#6A3D9A")
    text((s$cancer_followup_start + s$cancer_followup_end) / 2, 3.28, sprintf("Observed registry incidence: %d-%d", s$cancer_followup_start, s$cancer_followup_end), cex = 0.73, col = track_cols["CANCER"])
    text(s$primary_latency_break, 2.74, sprintf("Fixed +5-year breakpoint: %d", s$primary_latency_break), cex = 0.70, col = "#6A3D9A")
    axis(1, at = seq(1980, 2025, 5), labels = if (i == nrow(fig1)) seq(1980, 2025, 5) else FALSE, tck = -0.025, cex.axis = 0.75, col = "#666666", col.axis = "#444444")
  }
  mtext("Iodine-policy timing, population-UIC context, selected settings, and registry follow-up", side = 3, outer = TRUE, font = 2, cex = 1.25, line = 1.0)
  mtext("Year", side = 1, outer = TRUE, line = 2.0, cex = 0.9)
  mtext("Selected settings were retained for documented policy, population-UIC evidence, registry continuity, and long-term follow-up; no individual linkage is shown.", side = 1, outer = TRUE, line = 3.1, cex = 0.70, col = "#555555")
}
save_art(draw_figure1, file.path(out, "Figure_1"), 11, 8.5)

fig5 <- read.csv("Iodine_PublicP3C_Figure5_SourceData.csv", check.names = FALSE)
fig5$domain <- gsub(paste0("Biological", "\n", "UIC response"), paste0("Population-UIC", "\n", "corroboration"), fig5$domain, fixed = TRUE)
fig5$domain_group <- gsub("Exposure", "Policy and UIC context", fig5$domain_group, fixed = TRUE)
fig5$publication_category <- ifelse(grepl("UIC", fig5$domain), "population_UIC_corroboration_grade", fig5$domain_group)
write.csv(fig5, file.path(out, "Figure_5_SourceData.csv"), row.names = FALSE, na = "")

domain_levels <- unique(fig5$domain)
fig5$country <- factor(fig5$country, levels = rev(c("Australia", "New Zealand", "Croatia")))
fig5$domain <- factor(fig5$domain, levels = domain_levels)
group_cols <- c("Policy and UIC context" = "#E8F1F5", "Temporal incidence" = "#EDF3E7", "Mortality" = "#EEEEEE", "Histology" = "#F5EEE5", "Bias/context" = "#F5E7E9")
p5 <- ggplot(fig5, aes(domain, country, fill = domain_group)) +
  geom_tile(color = "white", linewidth = 1.3) +
  geom_text(aes(label = cell_label), family = "Helvetica", size = 3.05, lineheight = 0.92, color = "#222222") +
  scale_fill_manual(values = group_cols, guide = "none") +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL, title = "Cross-country qualitative evidence synthesis", subtitle = "Country-specific evidence categories; no pooled estimate", caption = "Cells report qualitative source-based categories, not numeric scores. Population-UIC corroboration is contextual and does not represent individual exposure.") +
  theme_bw(base_size = 9.5, base_family = "Helvetica") +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.text.x = element_text(angle = 33, hjust = 0, vjust = 0, size = 8.2, face = "bold"), axis.text.y = element_text(size = 9.2, face = "bold"), plot.title = element_text(face = "bold", size = 12), plot.subtitle = element_text(size = 9), plot.caption = element_text(size = 7.5, hjust = 0), plot.margin = margin(8, 12, 8, 8))
save_art(function() print(p5), file.path(out, "Figure_5"), 12, 5.2)

cat("P5B2_FIGURE1: DISPLAY-READY FROM P4F LOCKED SOURCE DATA\n")
cat("P5B2_FIGURE5: TERMINOLOGY-UPDATED FROM P3C LOCKED SOURCE DATA\n")
