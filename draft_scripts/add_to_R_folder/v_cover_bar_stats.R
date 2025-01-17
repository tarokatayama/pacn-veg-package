#' Stats Bar Plot for Understory Cover by Species, Life form, Nativity, or no grouping (all cover lumped)
#'
#' @inheritParams summarize_understory
#'
#' @param plant_grouping "None" = No Groups, all vegetation (native & non-native) is lumped together,
#' "Nativity" = Cover values lumped by native and non-native vegetation.
#' "Life_Form" = Cover values grouped into native and non-native lifeforms (e.g. Trees, Shrubs, Ferns).
#' "Species" = Cover values for individual plant species.
#'
#' @param measurement "Cover" = Total % Cover for plant_grouping.
#' "Chg_Prior = Difference between Cover for a selected cycle and the previous cycle.
#' "Years_Prior" = Number of years between sampling cycles.
#' "Chg_Per_Year" = Chg_Prior divided by Years_Prior.
#'
#' @return graph (gglot) of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' subalpine_mean_nativity_cover <- v_cover_bar_stats(community = "Subalpine Shrubland")
#'
#' }

v_cover_bar_stats <- function(combine_strata = FALSE, plant_grouping = "Nativity", species_filter,
                                      paired_change = FALSE, measurement = "Cover", park, sample_frame, community,
                                      year, cycle, plot_type, plot_number, filter_Code, silent = FALSE, return_n = FALSE) {


  # Set plant_grouping_vars used for calculating stats based on plant_grouping argument
  if (plant_grouping == "Species") {
    summary_param <- "Scientific_Name"
  } else if (plant_grouping == "None") {
    summary_param <- "Sampling_Frame"
  } else {
    summary_param <- plant_grouping

  }


  if (plant_grouping == "None") {
    plant_grouping_vars <- c()
  }

  if (plant_grouping == "Nativity") {
    plant_grouping_vars <- c("Nativity")
  }

  if (plant_grouping == "Life_Form") {
    plant_grouping_vars <- c("Nativity", "Life_Form")
  }

  if (plant_grouping == "Species") {
    plant_grouping_vars <- c("Nativity", "Life_Form", "Code", "Scientific_Name")
  }

  # Get raw data
  understory2 <- summarize_understory(combine_strata = combine_strata,
                                      plant_grouping = plant_grouping,
                                      paired_change = paired_change,
                                      park = park, sample_frame = sample_frame,
                                      community = community, year = year,
                                      cycle = cycle, plot_type = plot_type,
                                      plot_number = plot_number, silent = silent)

  if (!missing("species_filter") && plant_grouping == "Species") {
    understory2 <- understory2 %>%
      dplyr::filter(Scientific_Name %in% species_filter)
  } else if (!missing("species_filter") && plant_grouping != "Species") {
    stop("Invalid plant_grouping selection. If species_filter is used, plant_grouping must = 'Species'")
  } # may want to change this in the future - can filter species as part of summarize_understory,
  # then summarize by Life_form, Nativity, etc. This could be useful.



  if (plant_grouping != "None") {

    unknown_cover <- understory2 %>%
      dplyr::filter(.data[[summary_param]] == "Unknown" & "Cover" > 0)

    unk_cover_tot <- unknown_cover %>%
      dplyr::pull("Cover") %>%
      sum()

    understory3 <- understory2 %>%
      dplyr::filter(Stratum != "No_Veg",
                    Nativity != "Unknown")
    message(paste0(round(unk_cover_tot,2), "% cover of species with unknown ", plant_grouping, " removed"))
  } else {
    understory3 <- understory2
  }


  # Nativity discrete scale Colors:
  nativity_colors <- c("Native" = "#1b9e77",
                       "No_Veg" = "grey",
                       "Non-Native" = "#d95f02",
                       "Unknown" = "#7570b3")


  # add stats
  base_vars <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year", "Stratum")

  add_stats_vars <- c(base_vars, plant_grouping_vars)

  understory_stats <- add_stats(understory3,
                                dplyr::across(dplyr::all_of(add_stats_vars)))

  # sample size calculation for text (output is on graph caption)
  sample_size <- understory_stats %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == measurement) %>%
    dplyr::select(Sampling_Frame, Year, NPLOTS) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Sampling_Frame) %>%
    dplyr::mutate(count_cycles = match(Year, unique(Year))) %>%
    dplyr::mutate(SF = paste0("; ", Sampling_Frame, ": ")) %>%
    dplyr::mutate(SF = dplyr::case_when(count_cycles == 1 ~ SF,
                                        TRUE ~ "")) %>%
    dplyr::mutate(Text = paste0(SF, Year, " [n = ", NPLOTS, "]")) %>%
    dplyr::pull(Text) %>%
    paste(collapse = ", ") %>%
    stringr::str_sub(3) %>%
    stringr::str_replace_all(", ;", ";")
  sample_size

  if (return_n == TRUE){
    return(sample_size)
  }

  #........BAR YEARLY MEANS


  label_param <- stringr::str_replace_all(measurement, "_", " ")

  understory_stats <- understory_stats[complete.cases(understory_stats),]

  understory_stats <- understory_stats %>%
    dplyr::filter(Parameter == measurement)

  understory_stats[[summary_param]] <- as.factor(understory_stats[[summary_param]])

  understory_stats[[summary_param]] <- reorder(understory_stats[[summary_param]],
                                              understory_stats$MEAN, .fun = max,
                                              decreasing = TRUE)
  if (plant_grouping == "None") {
    understory_stats$Nativity <- "Unknown"
  }

  plot <- understory_stats %>%
    #forcats::fct_reorder(Scientific_Name, MEAN, .fun = max) %>%
    dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, " ", "_")) %>%
    dplyr::filter(NPLOTS != 0) %>%
    dplyr::filter(Parameter == measurement) %>%
    ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                           position=ggplot2::position_dodge(.9)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(y = paste(label_param, "(Total % Cover)")) +
    #ggh4x package allows nested facets:
    ggplot2::facet_grid(Stratum ~ .data[[summary_param]] + SF_no_space,
                        #labeller = ggplot2::label_parsed,
                        scales = "free_x") +
    ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
    ggplot2::xlab("Year") +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(caption = sample_size) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))

  num_facets <- understory_stats %>%
    dplyr::select(Sampling_Frame, .data[[summary_param]]) %>%
    dplyr::distinct()

  if (nrow(num_facets) > 6) {
    plot <- plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(.data[[summary_param]] + SF_no_space ~ ., switch = "y") +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0))
  }

  # This should be changed to if (distinct(Sampling_Frame) > 1)
  if (length(unique(understory_stats$Sampling_Frame)) > 2) {
    plot <- plot +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(SF_no_space ~ ., switch = "y", scales = "free_y") +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0))
    }


  return(plot)

}
