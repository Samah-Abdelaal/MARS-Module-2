# Word tables

library(flextable)
library(officer)
library(crosstable)

# participant ch.ch

(p <- crosstable(part_ch, cols = names(part_ch)) %>% 
  crosstable::as_flextable())#compact = getOption(
#"crosstable_compact", TRUE), 
#autofit = getOption("crosstable_autofit", FALSE)))
(p <- set_header_labels(p,
                        label = "Participant Characteristics",
                        variable = "",
                        value = "No (%)"))

(p <- fontsize(p, size = 10, part = "all"))
(p <- flextable::width(p, width = 2.5))
(p <- bold(p, j = 1))
#(p <- align(p, align = 'left', part = 'all'))
#(p <- border_remove(p))
#(p <- border_inner_h(p, border = fp_border(width = 2), part = 'header'))
(p <- border_inner_v(p, border = fp_border(width = 1)))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = p)
timeDate <- format(Sys.Date(), "%Y - %m - %d")
docx <- print(doc, target = paste('Participant-ChCh ', timeDate,
                                  '.docx', sep = ""))

# respect and satisfaction

resp_satis <- pca_DM[c(13:21)]

(r <- crosstable(resp_satis) %>% 
    crosstable::as_flextable())

(r <- set_header_labels(r,
                        label = "Respect & satisfaction on communication",
                        variable = "",
                        value = ""))
(r <- fontsize(r, size = 10, part = "all"))
(r <- flextable::width(r, width = 2.5))
(r <- bold(r, j = 1))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = r)
timeDate <- format(Sys.Date(), "%Y - %m - %d")
docx <- print(doc, target = paste('Respect & Satisfaction ', timeDate,
                                  '.docx', sep = ""))
#(r <- as.data.frame(unclass(table(resp_satis))))
#(r <- flextable(r))

#r <- pivot_longer(resp_satis, cols = names(resp_satis))
#t(r)

# openness and sharing

open_share <- pca_DM[c(22:30)]

(o <- crosstable(open_share) %>% 
    crosstable::as_flextable())


(o <- set_header_labels(o,
                        label = "Openness & sharing of information",
                        variable = "",
                        value = ""))
(r <- fontsize(r, size = 10, part = "all"))
(r <- flextable::width(r, width = 2.5))
(r <- bold(r, j = 1))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = r)
timeDate <- format(Sys.Date(), "%Y - %m - %d")
docx <- print(doc, target = paste('Openness & Sharing ', timeDate,
                                  '.docx', sep = ""))

# PCA data

(pc <- flextable(pca_all_data))

(pc <- fontsize(pc, size = 10, part = "all"))
(pc <- flextable::width(pc, width = 2))
(pc <- bold(pc, j = 1))
(pc <- bold(pc, part = "header"))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = pc)
timeDate <- format(Sys.Date(), "%Y - %m - %d")
docx <- print(doc, target = paste('PCA ', timeDate,
                                  '.docx', sep = ""))

# loading scores

(t <- flextable(as.data.frame(top_10_all)))

(t <- fontsize(t, size = 12, part = "header"))
(t <- flextable::width(t, width = 3))
(t <- align(t, align = "center", part = "all"))
(t <- set_header_labels(t,
                        top_10_all = "Top 10 Loading Scores"))
(t <- bold(t, part = "header"))
(t <- border_outer(t, border = fp_border(width = 2)))

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = t)
timeDate <- format(Sys.Date(), "%Y - %m - %d")
docx <- print(doc, target = paste('Loading Scores ', timeDate,
                                  '.docx', sep = ""))