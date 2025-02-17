#' Run Tasks with a Dynamic Worker Queue
#'
#' This function accepts a list of tasks (each a function with no arguments) and a specified number of worker processes.
#' It dynamically schedules tasks to available workers and processes their results in an event loop as they complete. Tasks
#' can be completed in parallel and the order of execution does not matter. However, the callbacks for post-processing tasks
#' must be run sequentially either in the order tasks are provided or in the reverse order of reverse is TRUE.
#' Parallel processing is set up using the future package and child processes are terminated properly once all tasks
#' are completed or if an error occurs.
#'
#' @param tasks A list of tasks to be executed. Each task should be a list with elements 'task' (a function with no arguments) and an optional 'callback' function to be executed upon task completion.
#' @param worker_count An integer specifying the number of simultaneous worker processes, defaults to 4.
#' @param reverse A logical indicating whether to execute callbacks in reverse order compared to the tasks order.
#' @param packages A vector of package names to be automatically loaded in each child process before executing tasks.
#' @param ... Additional arguments to be passed to the task functions.
#'
#' @return A list of results from the tasks, in the same order as provided.
#'
#' @details The function saves the current future plan and sets up a multisession plan with the desired number of workers.
#' It then restores the original plan upon exit, ensuring that any spawned sessions are terminated.
#'
#' @export
parallel_task_queue <- function(tasks, worker_count = 4, reverse = FALSE, packages = character(0), ...) {
  
  # Local logging function (following same pattern as word.R)
  log_info <- function(msg) {
    cat(sprintf("[%s] [parallel_task_queue] %s\n", Sys.time(), msg))
  }
  
  log_info(sprintf("Starting with %d task(s) and %d worker(s), reverse = %s", length(tasks), worker_count, reverse))
  
  # Save current future plan so it can be restored on exit.
  old_plan <- future::plan()
  on.exit({
    future::plan(old_plan)
    log_info("Restored the original future plan.")
  }, add = TRUE)
  
  # Set up multisession plan with the desired worker count.
  future::plan(future::multisession, workers = worker_count)
  log_info("Set multisession future plan.")
  
  dot_list <- list(...)
  
  n <- length(tasks)
  results <- vector("list", n)           # to store each task's result
  finished <- rep(FALSE, n)              # flag which tasks have finished
  callback_executed <- rep(FALSE, n)     # flag to mark that callback is executed
  
  pending_index <- 1                     # next task index to launch
  running <- list()                      # list of running futures
  
  # Set callback order pointer based on reverse setting.
  if (!reverse) {
    next_callback_index <- 1
  } else {
    next_callback_index <- n
  }
  
  # Event loop: continue as long as some tasks haven't been launched,
  # futures are still running, or callbacks have not been processed.
  while (pending_index <= n || length(running) > 0 || !all(callback_executed)) {
    
    # Launch tasks if there are free worker slots.
    while (length(running) < worker_count && pending_index <= n) {
      # Schedule the task as a future.
      current_task <- tasks[[pending_index]]
      task_fn <- current_task$task  # the function to run
      parent.env(environment(task_fn)) <- getNamespace("doclarative")
      # task_env <- new.env()
      # task_env$value <- tasks[[pending_index]]$value
      # environment(task_fn) <- task_env  # set minimal environment to avoid capturing large, unnecessary data
      log_info(sprintf("Submitting task %d.", pending_index))
      task_args <- c(list(.data = current_task$data), dot_list, current_task$valueFnData)
      fut <- future::future({
        result <- do.call(task_fn, task_args)
        result
      }, globals = c("task_fn", "task_args"), packages = packages, gc = TRUE)
      
      # Save the future along with its index and callback (if provided).
      running[[length(running) + 1]] <- list(
        id = pending_index,
        fut = fut,
        callback = if (!is.null(tasks[[pending_index]]$callback)) tasks[[pending_index]]$callback else NULL
      )
      
      pending_index <- pending_index + 1
    }
    
    # Check running futures for completion.
    new_running <- list()
    for (job in running) {
      if (future::resolved(job$fut, wait = FALSE)) {
        # Retrieve the result; if an error occurs, capture it.
        res <- tryCatch(future::value(job$fut),
                        error = function(e) e)
        results[[job$id]] <- res
        finished[job$id] <- TRUE
        log_info(sprintf("Task %d completed. Now printing result:", job$id))
        #print(res)
      } else {
        new_running[[length(new_running) + 1]] <- job
      }
    }
    running <- new_running
    
    # Execute callbacks in the sequential order.
    if (!reverse) {
      while (next_callback_index <= n && finished[next_callback_index] && !callback_executed[next_callback_index]) {
        cb <- tasks[[next_callback_index]]$callback
        if (!is.null(cb) && is.function(cb)) {
          log_info(sprintf("Executing callback for task %d.", next_callback_index))
          tryCatch({
            cb(results[[next_callback_index]])
          }, error = function(e) {
            log_info(sprintf("Error in callback for task %d: %s", next_callback_index, e$message))
            e
          })
          log_info(sprintf("Callback for task %d executed.", next_callback_index))
        } else {
          log_info(sprintf("No callback for task %d. Skipping.", next_callback_index))
        }
        callback_executed[next_callback_index] <- TRUE
        next_callback_index <- next_callback_index + 1
      }
    } else {
      while (next_callback_index >= 1 && finished[next_callback_index] && !callback_executed[next_callback_index]) {
        cb <- tasks[[next_callback_index]]$callback
        if (!is.null(cb) && is.function(cb)) {
          log_info(sprintf("Executing callback for task %d.", next_callback_index))
          tryCatch({
            cb(results[[next_callback_index]])
          }, error = function(e) {
            log_info(sprintf("Error in callback for task %d: %s", next_callback_index, e$message))
            e
          })
          log_info(sprintf("Callback for task %d executed.", next_callback_index))
        } else {
          log_info(sprintf("No callback for task %d. Skipping.", next_callback_index))
        }
        callback_executed[next_callback_index] <- TRUE
        next_callback_index <- next_callback_index - 1
      }
    }
    
    Sys.sleep(0.01)  # Brief pause to avoid busy waiting.
  }
  
  log_info("All tasks and callbacks processed. Returning results.")
  return(results)
}