chunk_list <- function(lst, n_chunks) {
    n_items <- length(lst)
    n_chunks <- min(n_chunks, n_items)
    
    if (n_items > 0) {
        # Calculate sizes for each chunk to distribute as evenly as possible
        chunk_sizes <- rep(floor(n_items/n_chunks), n_chunks)
        remainder <- n_items %% n_chunks
        if (remainder > 0) {
            chunk_sizes[1:remainder] <- chunk_sizes[1:remainder] + 1
        }
        
        # Split the list according to calculated sizes
        start_idx <- 1
        lapply(chunk_sizes, function(size) {
            chunk <- lst[start_idx:(start_idx + size - 1)]
            start_idx <<- start_idx + size
            chunk
        })
    } else {
        list()
    }
}



call_with_matching_args <- function(fn, args) {
    # Get the formal arguments of the function
    params <- names(formals(fn))
    
    # Check if the function accepts dots
    has_dots <- "..." %in% params
    
    if (has_dots) {
        # If function accepts dots, pass all arguments
        do.call(fn, args)
    } else {
        # Only pass arguments that match the function parameters
        matching_args <- args[intersect(names(args), params)]
        do.call(fn, matching_args)
    }
}

get_task_data <- function(spec_data, global_data, data_filter) {
    if (is.null(spec_data)) spec_data <- list()
    if (!is.list(spec_data)) spec_data <- list(spec_data)
    
    if (is.null(global_data)) global_data <- list()
    if (!is.list(global_data)) global_data <- list(global_data)
    
    if (is.null(data_filter)) data_filter <- function(...) list()
    filtered_data <- call_with_matching_args(data_filter, global_data)
    if (!is.list(filtered_data)) filtered_data <- list(filtered_data)
    
    combined_data <- modifyList(
        global_data,
        modifyList(filtered_data, spec_data)
    )
    combined_data
}

#' @export
run_report <- function(doc, output_path, spec_list, packages, n_workers = 4, ...) {
    dots <- list(...)

    for (i in seq_len(length(spec_list))) {
        spec_list[[i]]$.order <- i
        if (spec_list[[i]]$type == 'plot') {
            spec_list[[i]]$.temp_file <- paste0(tempfile(), '.png')
        }
    }
        
    plots <- Filter(function(spec) spec$type == 'plot', spec_list)

    n_plots <- length(plots)

    plot_gen_progress <- 15
    plot_add_progress <- 1
    total_steps <- n_plots * plot_gen_progress + length(spec_list) * plot_add_progress

    handlers(global = TRUE)
    handlers("cli")
    p <- progressr::progressor(steps = total_steps)
    plots_random_indices <- sample(seq_len(n_plots), n_plots, replace = FALSE)
    plots_random_sort <- plots[plots_random_indices]
    
    plan(multisession, workers = n_workers)
    
    # Process each plot with progress updates
    process_plot <- function(spec) {
        task_data <- get_task_data(spec$data, dots, spec$data_filter)
        plot <- call_with_matching_args(spec$value, task_data)
        ggsave(spec$.temp_file, plot, width = spec$width, height = spec$height, dpi = spec$res)
        p(amount = plot_gen_progress)
        spec$.temp_file
    }
    
    # Process all plots with parallel execution and progress updates
    future.apply::future_lapply(
        plots_random_sort,
        process_plot,
        future.chunk.size = max(1, length(plots) %/% n_workers),
        future.seed = TRUE,
        future.packages = packages,
        future.globals = c('dots', 'get_task_data', 'call_with_matching_args'),
        future.conditions = "message"
    )
    
    # Add images to document with progress updates
    for (spec in rev(spec_list)) {
        add_fn <- switch(
            spec$type,
            plot = add_plot,
            flextable = add_flextable,
            caption = add_caption,
            text = add_text,
            end_landscape_section = add_end_landscape_section,
            end_portrait_section = add_end_portrait_section,
            page_break = add_break
        )
        add_fn(doc, spec, dots)
        p(amount = plot_add_progress)
    }

    print(doc, output_path)
}

add_plot <- function(doc, spec, dots) {
    body_add_img(
        doc,
        spec$.temp_file,
        width = spec$width,
        height = spec$height
    )
}

add_flextable <- function(doc, spec, dots) {
    task_data <- get_task_data(spec$data, dots, spec$data_filter)
    ft <- call_with_matching_args(spec$value, task_data)
    body_add_flextable(
        doc,
        ft
    )
}

add_caption <- function(doc, spec, dots) {
    task_data <- get_task_data(spec$data, dots, spec$data_filter)
    if (class(spec$value) == 'character') {
        caption <- block_caption(spec$value)
    } else if (class(spec$value) == 'function') {
        caption <- block_caption(call_with_matching_args(spec$value, task_data))
    } else {
        stop('Invalid caption type')
    }
    body_add_caption(doc, caption)
}

add_text <- function(doc, spec, dots) {
    if (class(spec$value) == 'character') {
        text <- spec$value
    } else if (class(spec$value) == 'function') {  
        task_data <- get_task_data(spec$data, dots, spec$data_filter)
        text <- call_with_matching_args(spec$value, task_data)
    } else {
        stop('Invalid text type')
    }
    body_add_par(doc, text, style = spec$style)
}

add_end_landscape_section <- function(doc, spec, dots) {
    body_end_section_landscape(doc)
}

add_end_portrait_section <- function(doc, spec, dots) {
    body_end_section_portrait(doc)
}

add_break <- function(doc, spec, dots) {
    body_add_break(doc)
}
