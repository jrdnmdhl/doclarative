#' Create a parallel task for adding a paragraph to an officer document
#'
#' This function has the same interface as officer::body_add_par. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task evaluates the paragraph text and returns it, and the callback adds the paragraph to the document.
#'
#' @param x An officer document object.
#' @param value A character string or expression resulting in text.
#' @param ... Additional arguments passed to officer::body_add_par.
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_add_par <- function(x, value, ...) {
  list(
    data = list(value = value, ...),
    value = value,
    task = function(.data, ...) {
      dots <- list(...)
      if (is.function(.data$value)) {
        log_debug("Evaluating paragraph function")
        filtered_args <- filter_dot_args(.data$value, dots)
        res <- do.call(.data$value, filtered_args)
      } else {
        log_debug("Using literal paragraph object")
        res <- .data$value
      }
      res
    },
    callback = function(result) {
      body_add_par(x = x, value = result, ...)
    },
    data = list()
  )
}


#' Create a parallel task for adding a formatted paragraph (fpar) to an officer document
#'
#' This function has the same interface as officer::body_add_fpar. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task evaluates the formatted paragraph object and returns it, and the callback adds it to the document.
#'
#' @param x An officer document object.
#' @param value An fpar object or an expression resulting in an fpar.
#' @param ... Additional arguments passed to officer::body_add_fpar.
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_add_fpar <- function(x, value, ...) {
  list(
    data = list(value = value, ...),
    value = value,
    task = function(.data, ...) {
      dots <- list(...)
      if (is.function(.data$value)) {
        log_debug("Evaluating formatted paragraph function")
        filtered_args <- filter_dot_args(.data$value, dots)
        res <- do.call(.data$value, filtered_args)
      } else {
        log_debug("Using literal formatted paragraph object")
        res <- .data$value
      }
      res
    },
    callback = function(result) {
      body_add_fpar(x = x, value = result, ...)
    },
    data = list()
  )
}


#' Create a parallel task for adding a break to an officer document
#'
#' This function has the same interface as officer::body_add_break. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task simply returns the break position, and the callback adds the break to the document.
#'
#' @param x An officer document object.
#' @param ... Additional arguments passed to officer::body_add_break.
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_add_break <- function(x, ...) {
  list(
    data = list(...),
    value = NULL,
    task = function(.data, ...) {
      return(NULL)
    },
    callback = function(result) {
      body_add_break(x = x, ...)
    },
    data = list()
  )
}


#' Create a parallel task for adding a flextable to an officer document
#'
#' This function has the same interface as flextable::body_add_flextable. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task evaluates the flextable object and returns it, and the callback adds the flextable to the document.
#'
#' @param x An officer document object.
#' @param value A flextable object or an expression resulting in a flextable.
#' @param ... Additional arguments passed to flextable::body_add_flextable.
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_add_flextable <- function(x, value, ...) {
  list(
    data = list(value = value, ...),
    value = value,
    task = function(.data, ...) {
      dots <- list(...)
      if (is.function(.data$value)) {
        log_debug("Evaluating flextable function")
        filtered_args <- filter_dot_args(.data$value, dots)
        res <- do.call(.data$value, filtered_args)
      } else {
        log_debug("Using literal flextable object")
        res <- .data$value
      }
      res
    },
    callback = function(result) {
      body_add_flextable(x = x, value = result, ...)
    },
    data = list()
  )
}


#' Create a parallel task for adding a caption to an officer document
#'
#' This function has the same interface as officer::body_add_caption. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task evaluates the caption text and returns it, and the callback adds the caption to the document.
#'
#' @param x An officer document object.
#' @param value A caption text or an expression resulting in caption text.
#' @param ... Additional arguments passed to officer::body_add_caption
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_add_caption <- function(x, value, ...) {
  list(
    data = list(value = value, ...),
    value = value,
    task = function(.data, ...) {
      dots <- list(...)
      if (is.function(.data$value)) {
        log_debug("Evaluating caption function")
        filtered_args <- filter_dot_args(.data$value, dots)
        res <- do.call(.data$value, filtered_args)
      } else {
        log_debug("Using literal caption object")
        res <- .data$value
      }
      res
    },
    callback = function(result) {
      body_add_caption(x = x, value = block_caption(result), ...)
    }
  )
}


#' Create a parallel task for ending a section with portrait orientation in an officer document
#'
#' This function has the same interface as officer::body_end_section_portrait. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task does not perform a computation, and the callback ends the section with portrait orientation.
#'
#' @param x An officer document object.
#' @param ... Additional arguments passed to officer::body_end_section_portrait.
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_end_section_portrait <- function(x, ...) {
  list(
    data = list(...),
    task = function(.data, ...) {
      NULL
    },
    callback = function(result) {
      body_end_section_portrait(x = x, ...)
    },
    data = list()
  )
}


#' Create a parallel task for ending a section with landscape orientation in an officer document
#'
#' This function has the same interface as officer::body_end_section_landscape. It returns a task that can be executed in parallel using parallel_task_queue.
#' The task does not perform a computation, and the callback ends the section with landscape orientation.
#'
#' @param x An officer document object.
#' @param ... Additional arguments passed to officer::body_end_section_landscape.
#'
#' @return A list with elements 'task' and 'callback'.
#' @export
future_body_end_section_landscape <- function(x, ...) {
  list(
    data = list(...),
    task = function(.data, ...) {
      NULL
    },
    callback = function(result) {
      body_end_section_landscape(x = x, ...)
    },
    data = list()
  )
}


# Helper: Map a spec type to the corresponding future function
future_function_for_type <- function(type) {
  switch(type,
         caption = future_body_add_caption,
         flextable = future_body_add_flextable,
         text = future_body_add_par,
         formatted_text = future_body_add_fpar,
         page_break = future_body_add_break,
         plot = future_body_add_gg,
         end_portrait_section = future_body_end_section_portrait,
         end_landscape_section = future_body_end_section_landscape,
         element_list = NULL,  # Special case handled in body_add_from_spec
         stop(paste("Unsupported type:", type))
  )
}

# Helper: Filter dot_args based on the formal arguments of a function
filter_dot_args <- function(fun, dot_args) {
  sig <- formals(fun)
  nms <- names(dot_args)
  if (is.null(nms)) {
    return(dot_args)
  }
  # Separate unnamed (positional) arguments and named arguments
  unnamed <- dot_args[nms == ""]
  named <- dot_args[nms != ""]
  if ("..." %in% names(sig)) {
    return(dot_args)
  } else {
    matched <- named[names(named) %in% names(sig)]
    return(c(unnamed, matched))
  }
}

# Helper: Build the task (and record whether a value was supplied) from a spec
build_task <- function(spec, doc, dot_args) {
  log_debug("Building task for spec type: %s", spec$type)
  
  # Condition check
  if (!is.null(spec$condition) && is.function(spec$condition)) {
    log_debug("Evaluating condition function")
    if (!isTRUE(eval(as.call(c(list(spec$condition), filter_dot_args(spec$condition, dot_args)))))) {
      log_debug("Condition evaluated to FALSE, skipping task")
      return(NULL)
    }
  }

  # Base args: always include 'x'
  args_list <- c(
    list(x = doc),
    spec[!names(spec) %in% c("type", "condition", "data")]
  )

  future_func <- future_function_for_type(spec$type)
  log_debug("Creating task using %s", spec$type)
  task <- do.call(future_func, args_list)

  if (!is.null(spec$data)) {
    task$valueFnData <- spec$data
  } else {
    task$valueFnData <- list()
  }

  task
}

#' Create a list of parallel tasks from a specification
#'
#' @param doc An officer document object to be passed to each future function.
#' @param spec_list A list of lists, where each inner list specifies a task. Each inner list must have a 'type' property defining which future function to call (e.g. 'caption', 'flextable', 'text', 'plot', 'element_list').
#'        It may also include a 'value' property, which is a function that takes a data argument and ... arguments. The value function is wrapped so that the ... passed to body_add_from_spec are forwarded to it.
#'        Any additional properties will be passed as extra arguments to the future function.
#' @param ... Additional arguments that will be passed to the value functions of tasks requiring a 'value' argument.
#'
#' @return A list of task objects (each a list with 'task' and 'callback') that can be executed using parallel_task_queue.
#' @export
body_add_from_spec <- function(doc, spec_list, task_adder = NULL, ...) {
  log_info("Processing specification list with %d items", length(spec_list))
  dot_args <- list(...)
  task_tracker <- list(
    count = 0,
    tasks = vector(mode = 'list', length = 5000)
  )
  if (is.null(task_adder)) {
    task_adder <- function(...) {
      tasks_to_add <- list(...)
      n_tasks_to_add <- length(tasks_to_add)
      print(paste0('Adding task to index ', task_tracker$count + 1, ' to ', task_tracker$count + n_tasks_to_add))
      task_tracker$tasks[seq(from = task_tracker$count + 1, to = task_tracker$count + n_tasks_to_add, by = 1)] <<- tasks_to_add
      task_tracker$count <<- task_tracker$count + n_tasks_to_add
    }
  }
  lapply(spec_list, function(spec) {
    if (is.function(spec)) {
      log_debug("Evaluating function spec")
      spec <- do.call(spec, filter_dot_args(spec, dot_args))
      if (!is.list(spec)) {
        log_error("Spec function did not return a list")
        stop("Spec function must return a list.")
      }
    }

    if (!is.null(spec$type) && spec$type == "element_list") {
      log_debug("Processing element_list type")
      if (is.null(spec$items)) {
        log_error("element_list spec missing items")
        stop("element_list spec must include items")
      }
      nested_result <- body_add_from_spec(doc, spec$items, task_adder = task_adder, ...)
      return(nested_result$tasks)
    }

    res <- build_task(spec, doc, dot_args)

    if (!is.null(res)) {
      log_debug("Task built successfully")

      print(paste0('adding task of type ', spec$type))
      task_adder(res)
    }
    log_debug("Task build returned NULL")
  })
  
  tasks <- Filter(function(x) !is.null(x), task_tracker$tasks)
  log_info("Created %d tasks from specification", length(tasks))
  list(get_doc = function() doc, tasks = tasks)
}

#' Create a parallel task for rendering a ggplot and adding it as an image to an officer document
#'
#' This function has the same interface as body_add_gg from the officer package. Instead of directly rendering and adding
#' the ggplot, it returns a task that can be executed in parallel using parallel_task_queue. The task renders the ggplot,
#' saves it to a temporary png file using ggsave, and returns the path to the image. The callback then adds the image to the
#' document using body_add_img.
#'
#' @param x An officer document object.
#' @param value A ggplot expression that should not be immediately evaluated.
#' @param width The width of the saved image (default is 6 inches).
#' @param height The height of the saved image (default is 6 inches).
#' @param ... Additional arguments passed to both ggsave and body_add_img.
#'
#' @return A list with components 'task' and 'callback'. The 'task' function renders the ggplot and returns the image path,
#' and the 'callback' function adds the image to the document using the returned path.
#' @export
future_body_add_gg <- function(x, value, width = 6, height = 6, res = 300, ...) {
  task <- list(
    data = list(
      value = value,
      width = width,
      height = height,
      res = res,
      ...
    ),
    task = function(.data, ...) {
      dots <- list(...)
      if (is.function(.data$value)) {
        log_debug("Evaluating ggplot function")
        filtered_args <- filter_dot_args(.data$value, dots)
        g <- do.call(.data$value, filtered_args)
      } else {
        log_debug("Using literal ggplot object")
        g <- .data$value
      }
      
      # Ensure the plot is actually a ggplot object
      if (!inherits(g, "ggplot")) {
        stop("The evaluated expression did not return a ggplot object")
      }

      tmpfile <- tempfile(pattern = "doclarative_plot_", fileext = ".png")
      # Save the plot to the temporary file
      ggsave(filename = tmpfile, plot = g, width = .data$width, height = .data$height, dpi = .data$res)
      
      # Verify the file exists and is readable
      if (!file.exists(tmpfile)) {
        stop("Failed to create plot file at ", tmpfile)
      }
      
      # Return the path to the saved image
      return(tmpfile)
    },
    callback = function(result) {
      # Verify the file still exists before trying to add it
      if (!file.exists(result)) {
        stop("Plot file no longer exists at ", result)
      }
      
      # Add the image to the document using officer's body_add_img
      officer::body_add_img(x = x, src = result, width = width, height = height, ...)
      
      # Clean up the temporary file after it's been added to the document
      try(unlink(result), silent = TRUE)
    }
  )

  task
} 