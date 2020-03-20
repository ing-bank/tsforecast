#' Create stacked plot of hierarchical forecasts
#'
#' \code{plot_hierarchical_forecasts} is a function that creates a plot which
#' looks at how the forecasts of the chosen hierarchical group compares to it's
#' children, if applicable.
#'
#' @param main_forecasting_table A tibble containing a unique value for
#'   ts_split_date but not for the grouping column. It is assumed that this
#'   table is created using the \code{create_main_forecasting_table} function
#'   and which has been extended with the fc_models and fc_errors columns using
#'   the \code{add_fc_models_to_main_forecasting_table} and
#'   \code{add_hierarchical_fc_models_to_main_forecasting_table} functions.
#' @param fc_model A string that can be either "consistent" for consistent
#'   hierarchical forecast models or "bottom_up" for bottom-up hierarchical
#'   forecast models.
#' @param hierarchical_cols A string indicating which grouping columns are the
#'   hierarchical ones to be used in the plot.
#' @param grouping A string indicating which grouping is the top group of the
#'   stacked plot.
#' @param demo_mode Boolean, which is to be set to TRUE if any potentially
#'   sensitive figures should be hidden from the audience for demo purposes, or
#'   set to FALSE if all figures can safely be displayed.
#'
#' @return A plotly object displaying a stacked plot, if the selected grouping
#'   is a hierarchical parent.
#' @export
#'
#' @importFrom magrittr '%>%' extract2
#' @importFrom tidyr uncount
#' @import dplyr
#' @import ggplot2
#' @importFrom ggthemes theme_calc
#' @importFrom plotly ggplotly layout
#' @importFrom tibble tibble is_tibble
#' @importFrom tstools add_grouping_column check_data_format
#'   get_plot_colors period_to_last_day split_grouping_column
#'   unlist_if_required
#' @import Cairo
#'
#' @examples
#' dummy_hierarchical_gasprice %>%
#'       tstools::initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = "currency",
#'       hierarchical_cols = c("location", "oil_company")
#'    ) %>%
#'    dplyr::filter(period >= as.Date("2004-11-30")) %>%
#'    create_main_forecasting_table() %>%
#'    add_fc_models_to_main_forecasting_table(
#'       fc_methods = c("basic", "linear")
#'    ) %>%
#'    add_hierarchical_fc_models_to_main_forecasting_table() %>%
#'    dplyr::filter(ts_split_date == 200611) %>%
#'    plot_hierarchical_forecasts(
#'       fc_model = "consistent",
#'       hierarchical_cols = "location",
#'       grouping = "location = USA   &   oil_company = CompanyC   &   currency = EUR"
#'    )
plot_hierarchical_forecasts <- function(main_forecasting_table, fc_model = c("consistent", "bottom_up"), hierarchical_cols = "", grouping = "", demo_mode = FALSE) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "plot_hierarchical_forecasts",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "hierarchy", "fc_errors"
    ),
    unique_value_cols = "ts_split_date"
  )
  # Check fc_model
  fc_model <- match.arg(fc_model)
  # Check grouping
  all_groupings <- main_forecasting_table %>%
    dplyr::pull(grouping) %>% 
    unique()
  if (length(grouping) != 1) stop("The specified grouping does not have a length of one, as is required!")
  if (!grouping %in% all_groupings) stop("The specified grouping is not found in the main_forecasting_table!")
  # Check hierarchical_cols
  all_hierarchical_cols <- main_forecasting_table %>% 
    dplyr::filter(grouping == !! grouping) %>% 
    dplyr::pull(hierarchy) %>% 
    tstools::unlist_if_required() %>% 
    magrittr::extract2("matrix") %>% 
    comment()
  if (!all(hierarchical_cols %in% all_hierarchical_cols)) stop("The specified hierarchical_cols must be from the available hierarchical columns")

  # Get grouping information
  selected_grouping <- grouping
  grouping <- main_forecasting_table %>% 
    dplyr::filter(grouping == !! selected_grouping) %>% 
    tstools::split_grouping_column()
  # Get column names that are used for original main_forecasting_table grouping
  group_columns <- colnames(grouping)[!colnames(grouping) %in% colnames(main_forecasting_table)]
  # Get the tibble describing the hierarchical grouping
  hierarchical_group <- grouping %>% 
    dplyr::select(hierarchical_cols) 
  # Extract hierarchical data from hierarchy object
  hierarchical_data <- grouping %>% 
    dplyr::pull(hierarchy) %>% 
    tstools::unlist_if_required() %>% 
    magrittr::extract2("data")
  # Extract hierarchical matrix from hierarchy object
  hierarchical_matrix <- grouping %>% 
    dplyr::pull(hierarchy) %>% 
    tstools::unlist_if_required() %>% 
    magrittr::extract2("matrix")
  
  # Determine whether the group is an ultimate leaf or not
  all_leaves <- colnames(hierarchical_matrix)
  selected_hierarchical_grouping <- dplyr::inner_join(
      x = hierarchical_data,
      y = grouping %>% 
        dplyr::select(all_hierarchical_cols),
      by = all_hierarchical_cols
    ) %>% 
    dplyr::pull(grouping)
  is_leaf <- (selected_hierarchical_grouping %in% all_leaves)
  # Determine whether some of the hierarchical groups correspond to a leaf
  individual_leaves <- dplyr::inner_join(
      x = hierarchical_data,
      y = grouping %>% 
        dplyr::select(all_hierarchical_cols),
      by = all_hierarchical_cols
    ) %>% 
    dplyr::select(paste0("leaf_", hierarchical_cols)) %>%
    dplyr::distinct() %>% 
    dplyr::select_if(~ sapply(., sum) == 1) %>% 
    colnames() %>% 
    gsub("leaf_", "", .)
  
  # If is_leaf, or only one hierarchical col is selected
  if (is_leaf | (length(individual_leaves) == 1) & (length(hierarchical_cols) == 1)) {
    # Neutralize individual_leaves
    individual_leaves <- rep("", times = length(individual_leaves))
    # This means that is_leaf should definitely be TRUE
    is_leaf <- TRUE
  }

  # Determine which hierarchical groups are not leaves
  non_leaves <- colnames(hierarchical_group)[!colnames(hierarchical_group) %in% individual_leaves]
  # Get all children data from hierarchical_data
  colnames(hierarchical_group) <- paste0("parent_", colnames(hierarchical_group))
  all_children <- hierarchical_data %>% 
    dplyr::select(
      hierarchical_cols, 
      paste0("level_", hierarchical_cols), 
      paste0("parent_", hierarchical_cols)
    ) %>% 
    dplyr::distinct() %>% 
    dplyr::inner_join(
      x = .,
      y = hierarchical_group,
      by = paste0("parent_", non_leaves)
    ) %>% 
    dplyr::select(hierarchical_cols)
  colnames(hierarchical_group) <- gsub("parent_", "", colnames(hierarchical_group))
  
  # Filter out individual leaves in all_children
  if (length(individual_leaves) != 0 & !is_leaf) {
    all_children <- dplyr::inner_join(
      x = all_children,
      y = hierarchical_group %>% 
        dplyr::select(individual_leaves),
      by = individual_leaves
    )
  }
  # Bind rows to get all hierarchical groups that will be plotted
  hierarchical_groups_to_plot <- dplyr::bind_rows(
    hierarchical_group,
    all_children
  )
  
  # Get main_forecasting_table groupings for all groups to be plotted
  full_groupings <- grouping %>% 
    tidyr::uncount(nrow(hierarchical_groups_to_plot)) %>% 
    # Take out hierarchical_cols
    dplyr::select(-hierarchical_cols) %>% 
    dplyr::bind_cols(hierarchical_groups_to_plot) %>% 
    dplyr::select(colnames(grouping)) %>% 
    tstools::add_grouping_column(group_cols = group_columns) %>% 
    dplyr::pull(grouping)
  
  # Filter the main_forecasting_table according to these groups
  sub_mft <- main_forecasting_table %>% 
    dplyr::filter(grouping %in% full_groupings)
  # Bind rows of all fc_errors tibbles in this filtered main_forecasting_table
  fc_data <- sub_mft %>% 
    dplyr::pull(fc_errors) %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(grepl(!! fc_model, fc_model)) %>% 
    dplyr::transmute(
      period = tstools::period_to_last_day(period),
      value = fc_value,
      grouping = grouping
    )
  # Get historical data from filtered main_forecasting_table
  historical_data <- sub_mft %>%
    get_actuals_from_main_forecasting_table(for_plot = F) %>% 
    dplyr::rename(value = col_of_interest) %>% 
    dplyr::filter(!is.na(value)) %>% 
    dplyr::mutate(period = tstools::period_to_last_day(period))
  # Combine historical and fc data
  data <- historical_data %>% 
    # Get rid of historical data that crosses into forecasted values
    dplyr::filter(period < min(fc_data$period)) %>% 
    dplyr::bind_rows(fc_data) %>% 
    dplyr::arrange(period, grouping) %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(c(hierarchical_cols, "value", "period", "grouping")) %>% 
    dplyr::rename(master_grouping = grouping) %>% 
    tstools::add_grouping_column(
      group_cols = hierarchical_cols
    ) %>% 
    dplyr::left_join(
      x = hierarchical_groups_to_plot,
      y = .,
      by = hierarchical_cols
    )
  # Get ING plot colors
  colors <- tstools::get_plot_colors(n_colors = nrow(hierarchical_groups_to_plot))
  colors <- setNames(
    object = c("#000000", head(colors, -1)),
    nm = unique(data$grouping)
  )
  # Get split date line
  vertical_line_date <- main_forecasting_table %>%
    dplyr::pull(ts_split_date) %>% 
    unique() %>% 
    tstools::period_to_last_day()
  # Get parent data for line plot on top of stacked plot
  parent_data <- data %>% 
    dplyr::filter(master_grouping == selected_grouping)
  # Get children data, if applicable
  if (is_leaf) {
    area_plot_data <- data
  } else {
    area_plot_data <- data %>% 
      # Only start with children
      dplyr::filter(master_grouping != selected_grouping)
  }
  
  # Determine format function
  if (min(parent_data$value, na.rm = T) >= 0 & max(parent_data$value, na.rm = T) <= 1) {
    format_as_axis <- function(x) format(round(x, 2), nsmall = 0, big.mark = ",", scientific = F)
  } else {
    format_as_axis <- function(x) format(round(x, 0), nsmall = 0, big.mark = ",", scientific = F)
  }
  # Create tooltip text
  if (demo_mode) {
    tooltip_cols <- c("fill", "x")
  } else {
    tooltip_cols <- c("fill", "x", "y")
  }
  
  # Plot!
  plot <- ggplot2::ggplot(
      mapping = aes(x = period, y = value, fill = grouping, color = grouping)
    ) +
    ggplot2::geom_area(data = area_plot_data, size = .2, alpha = .4) +
    ggplot2::geom_line(data = parent_data) + 
    ggplot2::geom_vline(xintercept = as.numeric(vertical_line_date), linetype = 3) +
    ggplot2::scale_fill_manual(values = colors) + 
    ggplot2::scale_color_manual(values = colors) + 
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = format_as_axis) +
    ggthemes::theme_calc() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank()
    )
  # Hide y-axis if required
  if (demo_mode) {
    plot <- plot + 
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(), 
        axis.ticks.y = ggplot2::element_blank()
      )
  }
  # Make interactive and return
  plot %>% 
    plotly::ggplotly(tooltip = tooltip_cols) %>% 
    plotly::layout(legend = list(x = 100, y = 0.5)) %>% 
    return()
}