# Sentiment arc of Mary Shelley's Frankenstein: AFINN vs NRC
# ------------------------------------------------------------------------------
# Run from this folder:  Rscript frankenstein_sentiment.R
# Writes figures to figures/ and tables to output/. Week10A_CodeBase.qmd sources this file.
#
# First run only: tidytext::get_sentiments() asks you to accept each lexicon's
# licence in an interactive R session (AFINN: ODbL 1.0; NRC: commercial use needs
# a licence from its author). After that the lexicons are cached.

suppressPackageStartupMessages({
  library(gutenbergr); library(tidytext); library(dplyr); library(tidyr)
  library(stringr); library(purrr); library(ggplot2); library(ggrepel)
})

set.seed(1818)

# Analysis settings ------------------------------------------------------------
BANDWIDTH   <- 750    # SD of the Gaussian smoother, in words (spans roughly one chapter)
GRID_STEP   <- 100    # the smoothed curve is evaluated every 100 words
N_SHUFFLES  <- 200    # paragraph shuffles behind the turning-point threshold
N_BOOT      <- 2000   # bootstrap resamples for confidence intervals
MATCH_WORDS <- 1500   # turning points this close count as "the same" across lexicons

# Words that name or label a main character. They are removed from scoring because
# they would otherwise pre-load the comparison: NRC scores "victor" as positive and
# "creature", "monster", "fiend" and "wretch" as negative.
CHARACTER_LABELS <- c("victor", "frankenstein", "creature", "monster", "fiend",
                      "daemon", "dæmon", "demon", "devil", "wretch")

# 1. Text ----------------------------------------------------------------------
book_path <- "data/frankenstein_pg84.rds"
if (!file.exists(book_path)) {
  dir.create("data", showWarnings = FALSE)
  saveRDS(gutenberg_download(84, meta_fields = c("title", "author")), book_path)
}

raw <- readRDS(book_path)
body_start <- which(raw$text == "Letter 1")
stopifnot(length(body_start) == 1)   # the table of contents is indented, the heading is not

lines <- raw[body_start:nrow(raw), ] |>
  transmute(
    text = text |> str_replace_all("_", " ") |> str_replace_all("’", "'"),
    is_heading = str_detect(text, "^(Letter|Chapter) [0-9]+$"),
    para = cumsum(text == "")
  )
lines$section <- lines$text[lines$is_heading][cumsum(lines$is_heading)]

paragraphs <- lines |>
  filter(!is_heading, text != "") |>
  group_by(para, section) |>
  summarise(text = str_squish(str_c(text, collapse = " ")), .groups = "drop") |>
  arrange(para)

tokens <- paragraphs |>
  unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE) |>
  mutate(sent = row_number()) |>
  unnest_tokens(word, sentence, drop = FALSE) |>
  mutate(i = row_number())
N <- nrow(tokens)

section_levels <- unique(tokens$section)
section_start <- function(s) min(tokens$i[tokens$section == s])

# Word position of an exact phrase, so plot beats are anchored in the text itself.
locate <- function(phrase) {
  p <- str_split_1(str_to_lower(phrase), " ")
  hits <- which(tokens$word == p[1])
  for (k in seq_along(p)[-1]) hits <- hits[tokens$word[hits + k - 1] == p[k]]
  if (length(hits) < 1) stop("Anchor phrase not found: ", phrase)
  hits[1]
}

# 2. Plot beats and narrators ---------------------------------------------------
# Each beat runs until the next one begins, so every word belongs to exactly one.
beats <- tribble(
  ~beat,                                               ~start,
  "Walton sets out for the Arctic",                    section_start("Letter 1"),
  "Walton rescues Victor from the ice",                section_start("Letter 4"),
  "Happy childhood; Elizabeth joins the family",       section_start("Chapter 1"),
  "Mother dies; Victor leaves for university",         locate("Elizabeth had caught the scarlet fever"),
  "Victor's obsession with creating life",             section_start("Chapter 4"),
  "Creature brought to life; Victor flees",            locate("It was on a dreary night of November"),
  "Elizabeth's letter; recovery with Clerval",         section_start("Chapter 6"),
  "William murdered",                                  section_start("Chapter 7"),
  "Justine tried and condemned",                       section_start("Chapter 8"),
  "Victor's despair; Mont Blanc",                      section_start("Chapter 9"),
  "Victor confronts the Creature",                     locate("the figure of a man"),
  "Creature awakens; finds the De Lacey cottage",      section_start("Chapter 11"),
  "Creature learns from the De Laceys; Safie arrives", section_start("Chapter 12"),
  "Backstory of Safie and the De Laceys",              section_start("Chapter 14"),
  "Creature reads; the De Laceys reject him",          section_start("Chapter 15"),
  "Cottage burned; Creature shot after saving a girl", section_start("Chapter 16"),
  "Creature kills William; demands a mate",            locate("I grasped his throat to silence him"),
  "Victor agrees to build a mate",                     section_start("Chapter 17"),
  "Travels with Clerval: the Rhine and Britain",       section_start("Chapter 18"),
  "Victor destroys the mate; wedding-night threat",    section_start("Chapter 20"),
  "Clerval found dead; Victor jailed",                 section_start("Chapter 21"),
  "Victor returns home; wedding plans",                section_start("Chapter 22"),
  "Elizabeth murdered; Victor's father dies",          section_start("Chapter 23"),
  "Victor pursues the Creature to the Arctic",         section_start("Chapter 24"),
  "Walton's crew turns back; Victor dies",             locate("Walton in continuation"),
  "Creature mourns over Victor's body; vows to die",   locate("That is also my victim")
) |>
  arrange(start) |>
  mutate(end = lead(start, default = N + 1L) - 1L)

narrator_spans <- tribble(
  ~segment,               ~narrator,  ~start,
  "Walton (opening)",     "Walton",   1L,
  "Victor (Ch 1-10)",     "Victor",   section_start("Chapter 1"),
  "Creature (Ch 11-16)",  "Creature", section_start("Chapter 11"),
  "Victor (Ch 17-24)",    "Victor",   section_start("Chapter 17"),
  "Walton (closing)",     "Walton",   locate("Walton in continuation")
) |>
  mutate(end = lead(start, default = N + 1L) - 1L,
         segment = factor(segment, levels = segment))

tokens <- tokens |>
  mutate(
    segment  = narrator_spans$segment[findInterval(i, narrator_spans$start)],
    narrator = narrator_spans$narrator[findInterval(i, narrator_spans$start)],
    beat     = beats$beat[findInterval(i, beats$start)]
  )

# 3. Lexicons -------------------------------------------------------------------
afinn <- get_sentiments("afinn") |> rename(afinn = value)
nrc_all <- get_sentiments("nrc")
nrc <- nrc_all |>
  filter(sentiment %in% c("positive", "negative")) |>
  mutate(v = if_else(sentiment == "positive", 1L, -1L)) |>
  group_by(word) |>
  summarise(nrc = sum(v), .groups = "drop")   # a word tagged both ways nets to zero

tokens <- tokens |>
  left_join(afinn, by = "word") |>
  left_join(nrc, by = "word") |>
  mutate(
    is_label = word %in% CHARACTER_LABELS,
    afinn = if_else(is_label, 0, coalesce(as.numeric(afinn), 0)),
    nrc   = if_else(is_label, 0, coalesce(as.numeric(nrc), 0)),
    afinn_sign = sign(afinn)
  )

LEX <- c(afinn = "AFINN", nrc = "NRC")

# 4. Smoothing and turning points ------------------------------------------------
grid <- seq(1, N, by = GRID_STEP)

smooth_signal <- function(x, bw = BANDWIDTH) {
  nz <- which(x != 0); xz <- x[nz]
  map_dbl(grid, function(g) {
    lo <- max(1, g - 4 * bw); hi <- min(N, g + 4 * bw)
    keep <- nz >= lo & nz <= hi
    denom <- sum(dnorm(lo:hi, g, bw))   # renormalised so book ends are not pulled to zero
    1000 * sum(xz[keep] * dnorm(nz[keep], g, bw)) / denom
  })
}

# Local extrema with topographic prominence: how far a point stands out from the
# higher of the two valleys separating it from any more extreme point. If the walk
# in one direction reaches the edge of the book without meeting a more extreme
# point, that side is ignored: the book simply stops, which says nothing about how
# much the point stands out. Otherwise the opening and closing would be penalized.
find_extrema <- function(y) {
  n <- length(y)
  idx <- which(diff(sign(diff(y))) != 0) + 1
  map_dfr(idx, function(k) {
    is_peak <- y[k] > y[k - 1]
    yy <- if (is_peak) y else -y
    left_block  <- which(yy[seq_len(k - 1)] > yy[k])
    right_block <- which(yy[(k + 1):n] > yy[k])
    left_valley  <- if (length(left_block)) min(yy[max(left_block):k]) else NA_real_
    right_valley <- if (length(right_block)) min(yy[k:(k + min(right_block))]) else NA_real_
    base <- if (is.na(left_valley) && is.na(right_valley)) min(yy) else max(left_valley, right_valley, na.rm = TRUE)
    tibble(k = k, type = if (is_peak) "peak" else "trough", prominence = yy[k] - base)
  })
}

# What size of wiggle appears when the same paragraphs are in random order?
# threshold: the 95th percentile of wiggle sizes in a shuffled book (median over
# shuffles). A point above it stands out from routine wiggle.
para_index <- split(seq_len(N), tokens$para)
shuffle_threshold <- function(x, bw = BANDWIDTH) {
  null <- map(seq_len(N_SHUFFLES), function(s) {
    order_i <- unlist(para_index[sample(length(para_index))], use.names = FALSE)
    find_extrema(smooth_signal(x[order_i], bw))$prominence
  })
  median(map_dbl(null, quantile, 0.95))
}

TP_COLUMNS <- c("lexicon", "type", "pos", "section", "beat", "value", "prominence", "threshold", "passes")
turning_points <- function(lex, bw = BANDWIDTH) {
  y <- smooth_signal(tokens[[lex]], bw)
  find_extrema(y) |>
    mutate(pos = grid[k], value = y[k], lexicon = LEX[[lex]],
           threshold = shuffle_threshold(tokens[[lex]], bw),
           passes = prominence >= threshold,
           beat = tokens$beat[pos], section = tokens$section[pos]) |>
    select(all_of(TP_COLUMNS))
}

curves <- imap_dfr(LEX, ~ tibble(lexicon = .x, pos = grid, value = smooth_signal(tokens[[.y]]))) |>
  mutate(lexicon = as.character(lexicon)) |>
  group_by(lexicon) |> mutate(z = as.numeric(scale(value))) |> ungroup()

# The shuffles are slow, so results are cached. Delete this file after changing the
# settings, the character labels, or the text processing.
cache_path <- "output/turning_point_cache.rds"
if (file.exists(cache_path)) {
  cached <- readRDS(cache_path)
} else {
  dir.create("output", showWarnings = FALSE)
  cached <- list(all_extrema = map_dfr(c("afinn", "nrc"), turning_points))
  saveRDS(cached, cache_path)
}
all_extrema <- select(cached$all_extrema, all_of(TP_COLUMNS))
tp <- filter(all_extrema, passes)
set.seed(1831)   # later bootstraps give the same results whether or not the cache was used

# Pair up turning points of the same type across the two lexicons.
tp_afinn <- filter(tp, lexicon == "AFINN"); tp_nrc <- filter(tp, lexicon == "NRC")
tp_pairs <- map_dfr(seq_len(nrow(tp_afinn)), function(j) {
  a <- tp_afinn[j, ]
  cand <- filter(tp_nrc, type == a$type, abs(pos - a$pos) <= MATCH_WORDS)
  b <- if (nrow(cand)) cand[which.min(abs(cand$pos - a$pos)), ] else NULL
  tibble(type = a$type, afinn_pos = a$pos, afinn_beat = a$beat,
         nrc_pos = if (is.null(b)) NA_real_ else b$pos,
         nrc_beat = if (is.null(b)) NA_character_ else b$beat)
})
tp_pairs <- bind_rows(
  tp_pairs,
  tp_nrc |>
    filter(!pos %in% tp_pairs$nrc_pos) |>
    transmute(type, afinn_pos = NA_real_, afinn_beat = NA_character_, nrc_pos = pos, nrc_beat = beat)
) |>
  arrange(coalesce(afinn_pos, nrc_pos)) |>
  mutate(gap = abs(afinn_pos - nrc_pos))

# 5. Chapter-level results -------------------------------------------------------
per_1000 <- function(x, n) 1000 * sum(x) / n

by_section <- tokens |>
  group_by(section) |>
  summarise(start = min(i), words = n(),
            narrator = names(sort(table(narrator), decreasing = TRUE))[1],
            AFINN = per_1000(afinn, n()), NRC = per_1000(nrc, n()), .groups = "drop") |>
  arrange(start) |>
  mutate(short = str_replace(section, "Letter ", "L") |> str_replace("Chapter ", "Ch "))

negators <- c("not", "no", "never", "without", "nor", "neither", "cannot", "hardly", "scarcely")
sentiment_hits <- tokens |>
  mutate(prev = lag(word), prev2 = lag(word, 2),
         negated = prev %in% negators | prev2 %in% negators)

agreement <- list(
  n_words = N,
  coverage_afinn = mean(tokens$afinn != 0),
  coverage_nrc = mean(tokens$nrc != 0),
  book_afinn = per_1000(tokens$afinn, N),
  book_nrc = per_1000(tokens$nrc, N),
  chap_pearson = cor(by_section$AFINN, by_section$NRC),
  chap_spearman = cor(by_section$AFINN, by_section$NRC, method = "spearman"),
  curve_pearson = cor(curves$value[curves$lexicon == "AFINN"], curves$value[curves$lexicon == "NRC"]),
  sign_agree = mean(sign(by_section$AFINN) == sign(by_section$NRC)),
  n_sections = nrow(by_section),
  negated_afinn = mean(sentiment_hits$negated[sentiment_hits$afinn != 0]),
  negated_nrc = mean(sentiment_hits$negated[sentiment_hits$nrc != 0])
)

# Book-level score on the words both lexicons score.
shared_hit <- tokens$afinn != 0 & tokens$nrc != 0
levels_check <- tibble(
  measure = "Words both lexicons score",
  AFINN = per_1000(tokens$afinn[shared_hit], N),
  AFINN_sign_only = per_1000(tokens$afinn_sign[shared_hit], N),
  NRC = per_1000(tokens$nrc[shared_hit], N)
)

# Vocabulary comparison: how many distinct words does each lexicon score here?
vocab <- tibble(types_afinn = n_distinct(tokens$word[tokens$afinn != 0]),
                types_nrc = n_distinct(tokens$word[tokens$nrc != 0]))

# 6. Character focus --------------------------------------------------------------
boot_ci <- function(score, unit, n_boot = N_BOOT) {
  d <- tibble(score, unit) |>
    group_by(unit) |>
    summarise(s = sum(score), n = n(), .groups = "drop")
  stats <- replicate(n_boot, { k <- sample(nrow(d), replace = TRUE); 1000 * sum(d$s[k]) / sum(d$n[k]) })
  tibble(mean = 1000 * sum(d$s) / sum(d$n), lo = quantile(stats, .025), hi = quantile(stats, .975))
}

segment_scores <- tokens |>
  pivot_longer(c(afinn, nrc), names_to = "lex", values_to = "score") |>
  mutate(lexicon = LEX[lex]) |>
  group_by(lexicon, segment, narrator) |>
  group_modify(~ boot_ci(.x$score, .x$para)) |>
  ungroup()

# Difference between two token subsets, resampling paragraphs within each group.
diff_ci <- function(score, unit, group, a, b, n_boot = N_BOOT) {
  d <- tibble(score, unit, group) |>
    filter(group %in% c(a, b)) |>
    group_by(group, unit) |>
    summarise(s = sum(score), n = n(), .groups = "drop")
  da <- filter(d, group == a); db <- filter(d, group == b)
  r <- function(x, k) 1000 * sum(x$s[k]) / sum(x$n[k])
  stats <- replicate(n_boot, r(da, sample(nrow(da), replace = TRUE)) - r(db, sample(nrow(db), replace = TRUE)))
  tibble(diff = r(da, seq_len(nrow(da))) - r(db, seq_len(nrow(db))),
         lo = quantile(stats, .025), hi = quantile(stats, .975))
}

# Victor minus Creature, for the whole book and with the De Lacey chapters removed.
scenario <- function(name, data) {
  map_dfr(c(AFINN = "afinn", NRC = "nrc"), function(col) {
    diff_ci(data[[col]], data$para, data$narrator, "Victor", "Creature")
  }, .id = "lexicon") |> mutate(scenario = name, .before = 1)
}

character_sensitivity <- bind_rows(
  scenario("Main analysis: strict narrator, labels removed", tokens),
  scenario("Creature's De Lacey and Safie chapters (Ch 12-14) removed",
           filter(tokens, !beat %in% c("Creature learns from the De Laceys; Safie arrives",
                                       "Backstory of Safie and the De Laceys")))
)

# Victor's sentences that name the Creature, vs his other sentences. Only unambiguous
# references: "creature" also describes Elizabeth and "wretch" is often self-description.
sentences <- tokens |>
  filter(narrator == "Victor") |>
  group_by(sent) |>
  summarise(words = list(word), n = n(), afinn = sum(afinn), nrc = sum(nrc), .groups = "drop") |>
  mutate(
    names_creature = map_lgl(words, ~ any(.x %in% c("monster", "fiend", "dæmon", "daemon", "demon"))),
    view = if_else(names_creature, "Victor, on the Creature", "Victor, on everything else")
  )

perspective <- sentences |>
  pivot_longer(c(afinn, nrc), names_to = "lex", values_to = "score") |>
  mutate(lexicon = LEX[lex]) |>
  group_by(lexicon, view) |>
  group_modify(function(.x, ...) {
    st <- replicate(N_BOOT, { k <- sample(nrow(.x), replace = TRUE); 1000 * sum(.x$score[k]) / sum(.x$n[k]) })
    tibble(sentences = nrow(.x), words = sum(.x$n), mean = 1000 * sum(.x$score) / sum(.x$n),
           lo = quantile(st, .025), hi = quantile(st, .975))
  }) |>
  ungroup()

# NRC's eight emotions
emotions <- c("anger", "fear", "disgust", "sadness", "anticipation", "joy", "surprise", "trust")
nrc_emotions <- filter(nrc_all, sentiment %in% emotions)
emotion_hits <- tokens |>
  filter(!is_label) |>
  select(word, narrator) |>
  inner_join(nrc_emotions, by = "word", relationship = "many-to-many")

narrator_words <- count(tokens, narrator, name = "total")
emotion_by_narrator <- emotion_hits |>
  count(narrator, sentiment) |>
  left_join(narrator_words, by = "narrator") |>
  mutate(rate = 1000 * n / total)
# 7. Figures ----------------------------------------------------------------------
INK <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#8a8984"; GRIDLINE <- "#e7e6e1"; SURFACE <- "#fcfcfb"
LEX_COL <- c(AFINN = "#2a78d6", NRC = "#eb6834")
NARR_COL <- c(Victor = "#1baf7a", Creature = "#6250d6", Walton = "#eda100")

theme_arc <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.background = element_rect(fill = SURFACE, colour = NA),
          panel.background = element_rect(fill = SURFACE, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = GRIDLINE, linewidth = 0.3),
          text = element_text(colour = INK),
          axis.text = element_text(colour = INK2),
          axis.title = element_text(colour = INK2),
          plot.title = element_text(face = "bold", size = rel(1.25)),
          plot.title.position = "plot",
          plot.subtitle = element_text(colour = INK2, margin = margin(b = 10)),
          plot.caption = element_text(colour = MUTED, hjust = 0, margin = margin(t = 10)),
          plot.caption.position = "plot",
          strip.text = element_text(face = "bold", colour = INK, hjust = 0),
          legend.position = "top", legend.justification = "left",
          plot.margin = margin(14, 18, 10, 14))
}

save_fig <- function(p, name, w = 13, h = 7.5) {
  ggsave(file.path("figures", paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = SURFACE)
}

chapter_breaks <- c("Letter 1", paste("Chapter", c(1, 5, 10, 15, 20, 24)))
x_breaks <- map_dbl(chapter_breaks, section_start)
x_labels <- c("Letter 1", "Ch 1", "Ch 5", "Ch 10", "Ch 15", "Ch 20", "Ch 24")
scale_book_x <- function() {
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = expansion(mult = c(0.005, 0.005)))
}

band_data <- narrator_spans |>
  mutate(mid = (start + end) / 2,
         band_label = recode(narrator, Walton = "Walton", Victor = "Victor narrates", Creature = "Creature narrates"))

plot_arc <- function(lex) {
  crv <- filter(curves, lexicon == lex)
  pts <- filter(tp, lexicon == lex) |>
    mutate(label = str_c(str_replace(section, "Chapter ", "Ch "), ": ", str_wrap(beat, 26)))
  rng <- range(crv$value); span <- diff(rng)
  top <- rng[2] + 0.46 * span; bottom <- rng[1] - 0.34 * span
  ggplot() +
    geom_rect(data = band_data, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = narrator),
              alpha = 0.10, colour = NA) +
    geom_text(data = band_data, aes(x = mid, y = top, label = band_label), size = 3.5,
              colour = INK2, fontface = "bold", vjust = 1) +
    geom_hline(yintercept = 0, colour = MUTED, linewidth = 0.4) +
    geom_line(data = crv, aes(pos, value), colour = LEX_COL[[lex]], linewidth = 0.9) +
    geom_point(data = pts, aes(pos, value), shape = 21, size = 3.4, stroke = 1.1,
               fill = LEX_COL[[lex]], colour = SURFACE) +
    geom_text_repel(data = filter(pts, type == "peak"), aes(pos, value, label = label),
                    size = 3.25, lineheight = 0.92, colour = INK, nudge_y = 0.19 * span, direction = "both",
                    segment.colour = MUTED, segment.size = 0.35, box.padding = 0.4, point.padding = 0.4,
                    min.segment.length = 0, seed = 7, max.overlaps = Inf, bg.color = SURFACE, bg.r = 0.12) +
    geom_text_repel(data = filter(pts, type == "trough"), aes(pos, value, label = label),
                    size = 3.25, lineheight = 0.92, colour = INK, nudge_y = -0.16 * span, direction = "both",
                    segment.colour = MUTED, segment.size = 0.35, box.padding = 0.4, point.padding = 0.4,
                    min.segment.length = 0, seed = 7, max.overlaps = Inf, bg.color = SURFACE, bg.r = 0.12) +
    scale_fill_manual(values = NARR_COL, guide = "none") +
    scale_book_x() +
    coord_cartesian(ylim = c(bottom, top), clip = "off") +
    labs(x = "Position in the novel (ticks mark chapter openings)",
         y = "Net sentiment per 1,000 words (smoothed)",
         title = str_c(lex, " sentiment across Frankenstein"),
         subtitle = str_c(
           if (lex == "AFINN") "Word scores from -5 to +5" else "Positive words minus negative words",
           ". Labeled points stand out from the wiggle seen when the paragraphs are shuffled."),
         caption = "Gaussian smoother, SD 750 words. Character names and labels are excluded from scoring.") +
    theme_arc() +
    theme(panel.grid.major.y = element_line(colour = GRIDLINE, linewidth = 0.3),
          panel.grid.major.x = element_line(colour = GRIDLINE, linewidth = 0.3))
}

fig_overlay <- ggplot(curves, aes(pos, z, colour = lexicon)) +
  geom_rect(data = band_data, inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = narrator),
            alpha = 0.10, colour = NA) +
  geom_hline(yintercept = 0, colour = MUTED, linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_text(data = band_data, inherit.aes = FALSE, aes(x = mid, y = Inf, label = band_label),
            size = 3.4, colour = INK2, fontface = "bold", vjust = 1.5) +
  geom_text(data = curves |> group_by(lexicon) |> slice_max(pos, n = 1, with_ties = FALSE),
            aes(label = lexicon), hjust = -0.2, size = 3.6, colour = INK, fontface = "bold") +
  scale_colour_manual(values = LEX_COL, name = NULL) +
  scale_fill_manual(values = NARR_COL, guide = "none") +
  scale_book_x() +
  coord_cartesian(clip = "off") +
  labs(x = "Position in the novel (ticks mark chapter openings)",
       y = "Smoothed sentiment (SDs from own mean)",
       title = "The two lexicons, put on a common scale",
       subtitle = "Each curve is standardized, so shape can be compared but height differences are not raw score differences.") +
  theme_arc() + theme(plot.margin = margin(14, 50, 10, 14))

label_sections <- by_section |>
  mutate(residual = resid(lm(NRC ~ AFINN, data = by_section))) |>
  slice_max(abs(residual), n = 7) |>
  pull(section)
fig_scatter <- ggplot(by_section, aes(AFINN, NRC)) +
  geom_hline(yintercept = 0, colour = MUTED, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = MUTED, linewidth = 0.6, linetype = "22") +
  geom_point(aes(fill = narrator), shape = 21, size = 3.8, stroke = 0.9, colour = SURFACE) +
  geom_text_repel(data = filter(by_section, section %in% label_sections | short %in% c("Ch 8", "Ch 1")),
                  aes(label = short), size = 3.4, colour = INK, min.segment.length = 0.2, seed = 3,
                  segment.colour = MUTED, box.padding = 0.45) +
  scale_fill_manual(values = NARR_COL, name = "Main narrator") +
  labs(x = "AFINN net sentiment per 1,000 words",
       y = "NRC net sentiment per 1,000 words",
       title = "Chapter by chapter, the lexicons mostly rank the book alike",
       subtitle = "Chapters and story beats score similarly for both lexicons") +
  guides(fill = guide_legend(override.aes = list(size = 4.5))) +
  theme_arc()

segment_plot <- segment_scores |>
  mutate(segment = factor(segment, levels = rev(levels(narrator_spans$segment))),
         lexicon = factor(lexicon, levels = c("AFINN", "NRC")))
fig_narrator <- ggplot(segment_plot, aes(mean, segment, colour = narrator)) +
  geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.4) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.9) +
  geom_point(size = 3.8) +
  geom_text(aes(label = sprintf("%+.1f", mean)), vjust = -1.1, size = 3.3, colour = INK) +
  facet_wrap(~ lexicon, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = 0.06)) +
  scale_colour_manual(values = NARR_COL, name = "Narrator") +
  labs(x = "Net sentiment per 1,000 words (dot: estimate; line: 95% paragraph bootstrap interval)", y = NULL,
       title = "Sentiment by narrator, in the order the book tells it",
       subtitle = "The two panels use different scales because the lexicons score differently.") +
  theme_arc() + theme(panel.grid.major.y = element_blank(), panel.spacing.x = unit(2.5, "lines"))

emotion_plot <- emotion_by_narrator |>
  filter(narrator %in% c("Victor", "Creature")) |>
  select(narrator, sentiment, rate) |>
  mutate(sentiment = factor(sentiment, levels = rev(emotions)))
emotion_wide <- pivot_wider(emotion_plot, names_from = narrator, values_from = rate)
fig_emotion <- ggplot() +
  geom_segment(data = emotion_wide, aes(x = Victor, xend = Creature, y = sentiment, yend = sentiment),
               colour = MUTED, linewidth = 0.7) +
  geom_point(data = emotion_plot, aes(rate, sentiment, colour = narrator), size = 4) +
  geom_text(data = emotion_wide, aes(x = pmax(Victor, Creature), y = sentiment,
                                     label = sprintf("%+.1f", Creature - Victor)),
            hjust = -0.6, size = 3.3, colour = INK2) +
  scale_colour_manual(values = NARR_COL, name = NULL) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.1))) +
  labs(x = "NRC emotion words per 1,000 words", y = NULL,
       title = "NRC emotions: Victor's narration vs the Creature's",
       subtitle = "The number at the right of each row is Creature minus Victor.") +
  theme_arc() + theme(panel.grid.major.y = element_blank())

dir.create("figures", showWarnings = FALSE); dir.create("output", showWarnings = FALSE)
figures <- list(fig1_afinn_arc = plot_arc("AFINN"), fig2_nrc_arc = plot_arc("NRC"),
                fig3_lexicon_overlay = fig_overlay, fig4_chapter_scatter = fig_scatter,
                fig5_narrators = fig_narrator, fig6_emotions_narrator = fig_emotion)
sizes <- list(fig3_lexicon_overlay = c(13, 6), fig4_chapter_scatter = c(9.5, 7.5), fig5_narrators = c(12, 5),
              fig6_emotions_narrator = c(9.5, 5.5))
iwalk(figures, function(p, nm) {
  s <- if (is.null(sizes[[nm]])) c(13, 7.5) else sizes[[nm]]
  save_fig(p, nm, s[1], s[2])
})

# 8. Tables -----------------------------------------------------------------------
write.csv(tp, "output/turning_points.csv", row.names = FALSE)
write.csv(by_section, "output/section_scores.csv", row.names = FALSE)
write.csv(select(beats, beat, start_word = start, end_word = end), "output/plot_beats.csv", row.names = FALSE)
write.csv(segment_scores, "output/narrator_scores.csv", row.names = FALSE)
