library(tidyverse)
library(nnet)
theme_set(
	theme_minimal() +
		theme(legend.position = "bottom",
			    panel.spacing = unit(1, "lines"))
)

d <- read_csv(here::here("data", "options.csv"), 
							na = c("NULL", "null", "N/A", "")) %>%
	janitor::clean_names() %>%
	drop_na(ssid) %>%
	filter(form != "ORora") %>%
	separate(form, c("content", "grade")) %>%
	mutate(grade = parse_number(grade))

by_form <- d %>%
	group_by(content, grade) %>%
	nest()

tmp <- by_form$data[[1]] %>%
	group_by(ssid) %>%
	mutate(raw_score = sum(score, na.rm = TRUE)) %>%
	ungroup()

prep_data <- function(d, var, var_ref) {
	v <- enquo(var)
	d <- d %>%
		filter(
			(answer == "A" | answer == "B" | answer == "C") &
			 raw_score < 48
		) 
	
	correct_response <- as.character(unique(d$answer[d$score == 1]))

	d <- d %>%
		mutate(answer = relevel(factor(answer), ref = correct_response),
			     !!v := relevel(factor(!!v), ref = var_ref)) %>%
		select(ssid, !!v, item_num, answer, raw_score)

	comment(d) <- c("correct_response" = correct_response)
	d
}

fit_model <- function(d) {
	form <- formula(
						paste0("answer ~ raw_score*", names(d)[2]),
						data = d)

	m <- multinom(form, 
		            data = d, 
		            family = binomial(link = "logit"))
	m$correct_response <- comment(d)
	m
}
m <- fit_model(dd)

pull_probs <- function(m) {
	trms <- as.character(attr(m$terms, "variables"))[3:4]
	ef <- effects::Effect(trms,
		                    m, 
									      xlevels = list(raw_score = 0:48))

	mm <- ef$model.matrix
	mm <- mm[ ,-c(1, grep(":", colnames(mm)))]

	probs <- data.frame(mm, 
		                  ef$prob, 
		                  ef$lower.prob, 
	                    ef$upper.prob) %>%
	as_tibble() %>%
	rename(p.prob.A = prob.A,
				 p.prob.B = prob.B,
				 p.prob.C = prob.C) %>%
	gather(term, prob, matches("\\.")) %>%
	separate(term, c("est", NA, "option")) %>%
	spread(est, prob) %>%
	mutate(option = ifelse(option == m$correct_response, 
		     	               paste("Option", option, "(correct response)"), 
			     	             paste("Option", option)))

	levs <- unique(
	c(
		grep(" A", probs$option, value = TRUE),
		grep(" B", probs$option, value = TRUE),
		grep(" C", probs$option, value = TRUE)
		)
	)

	probs$option <- factor(probs$option, levels = levs)

	tbl_check <- table(substr(names(probs), 1, 5))

	if(any(tbl_check > 1)) {
		probs <- probs %>%
			gather(!!sym(trms[2]), dummy, 
				     starts_with(names(tbl_check)[tbl_check > 1])) %>%
			filter(dummy == 1) %>%
			select(-dummy)
	}
	else {
		lbl <- colnames(mm)[2]
		label <- substr(lbl, nchar(lbl), nchar(lbl))
		new_col_name <- substr(lbl, 1, nchar(lbl) - 1)

		names(probs)[grepl(lbl, names(probs))] <- new_col_name

		probs <- probs %>%
			mutate(!!sym(new_col_name) := ifelse(
				!!sym(new_col_name) == 1, label, "M") # Fix this so it's more general
			)
	}
	probs
	# probs %>%
	# 	select(6, 1:2, estimate = p, lower_bound = L, upper_bound = U)
}


pull_probs(m)

draw_plot <- function(p, label) {
	ggplot(p, aes(raw_score, estimate, color = !!sym(names(p)[1]))) +
		geom_ribbon(aes(ymin = lower_bound, 
			              ymax = upper_bound, 
			              fill = !!sym(names(p)[1])), 
			          color = NA,
			          alpha = 0.2) +
		geom_line() +
		facet_wrap(~option) +
		colorblindr::scale_color_OkabeIto(name = stringr::str_to_title(label)) +
		colorblindr::scale_fill_OkabeIto(name = stringr::str_to_title(label)) +
		labs(x = "Raw Score", 
			   y = "Probability",
			   title = paste0("Probability of option selection by", tolower(label)))
}
tmp %>% 
	filter(item_num == 1) %>%
	prep_data(idea_elig_code1, "10") %>%
	fit_model() %>%
	pull_probs() %>%
	draw_plot("Gender")


