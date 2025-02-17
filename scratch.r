library(dplyr)
library(purrr)
library(doclarative)
library(officer)

spec_list <- list(
  list(type = 'text', value = 'foo'),
  list(type = 'caption', value = 'bar'),
  list(type = 'plot', value = function(data) ggplot(aes(x=mpg,y=disp), data = data) + geom_point()),
  function(data) {
    items <- mtcars %>%
      group_by(cyl) %>%
      group_split() %>%
      map(function(x) {
        list(type = 'plot', value = function(data) ggplot(aes(x=mpg,y=disp), data = filter(data, cyl == x$cyl[1])) + geom_point())
      })
    list(
      type = 'element_list',
      items = items
    )
  }
)

doc <- officer::read_docx('~/Documents/Code/rtte2/inst/templates/word_template.docx')
tasks_and_doc_getter = doclarative::body_add_from_spec(doc, spec_list, data = mtcars)
foo <- doclarative::parallel_task_queue(
  tasks_and_doc_getter$tasks,
  reverse = TRUE,
  packages = c('ggplot2', 'dplyr', 'rtte2', 'doclarative', 'officer', 'flextable', 'tibble', 'stringr'),
  worker_count = 4,
  data = mtcars
)
print(tasks_and_doc_getter$get_doc(), '~/downloads/doclarative_test.docx')