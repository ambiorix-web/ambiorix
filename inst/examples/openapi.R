# openapi (swagger) docs example
#   http://localhost:3000/docs         -> interactive swagger ui
#   http://localhost:3000/openapi.json -> the raw openapi 3.1 document
#
# run it with:
#   source(system.file("examples", "openapi.R", package = "ambiorix"))

library(ambiorix)

# todolist in an in-memory "database":
TodoList <- function() {
  self <- new.env(parent = emptyenv())

  #' Generate Random Task IDs
  #'
  #' @param n Integer /// Optional.
  #'          Number of IDs to generate. \cr
  #'          Defaults to `1L`.
  #'
  #' @return Character vector.
  #'
  #' @examples
  #' generate_id()
  #' generate_id(n = 3L)
  generate_id <- function(n = 1L) {
    chars <- c(letters, LETTERS, 0:9)

    ids <- character(length = n)

    for (idx in seq_len(n)) {
      ids[[idx]] <- paste(
        sample(x = chars, size = 10, replace = TRUE),
        collapse = ""
      )
    }

    ids
  }

  self$tasks <- list()

  # add 3 tasks:
  ids <- generate_id(n = 3L)
  titles <- c("write docs", "add tests", "fix json bug")
  done <- c(TRUE, FALSE, FALSE)
  # a lone tag is an array of one, and no tags is an empty array: both
  # serialize as the array the `Task` schema documents
  tags <- list(
    I("docs"),
    c("qa", "ci"),
    list()
  )

  for (idx in 1:3) {
    id <- ids[[idx]]

    self$tasks[[id]] <- list(
      id = id,
      title = titles[[idx]],
      done = done[[idx]],
      tags = tags[[idx]]
    )
  }

  # return a list of methods:
  list(
    #' Read Specific Task
    #'
    #' @param id String /// Required.
    #'           ID of the task to read.
    #'
    #' @return Either:
    #'          - Named list: details of the task, or,
    #'          - `NULL` if a task with such `id` was not found.
    read_task = function(id) {
      self$tasks[[id]]
    },

    #' Read All Tasks
    #'
    #' @return A named list of named lists.
    read_all_tasks = function() {
      self$tasks
    },

    #' Create New Task
    #'
    #' @param title String /// Required. \cr
    #'              Title of the task.
    #'
    #' @param done Logical /// Optional. \cr
    #'             Is the task already done? \cr
    #'             Defaults to `FALSE`.
    #'
    #' @param tags Character vector /// Optional. \cr
    #'             Tags for this task. \cr
    #'             Defaults to `list()`, no tags.
    #'
    #' @return Named list. The new task.
    create_task = function(title, done = FALSE, tags = list()) {
      id <- generate_id()

      new_task <- list(
        id = id,
        title = title,
        done = done,
        tags = tags
      )

      self$tasks[[id]] <- new_task

      new_task
    },

    #' Update Task
    #'
    #' If a field has value `NULL` (default), it will not be updated.
    #'
    #' @param id String /// Required. \cr
    #'           Task ID.
    #'
    #' @param title String /// Optional. \cr
    #'              New title of the task.
    #'
    #' @param done Logical /// Optional. \cr
    #'             Is the task already done?
    #'
    #' @param tags Character vector /// Optional. \cr
    #'             New tags for the task.
    #'
    #' @return Either:
    #'          - Named list: The updated task, or,
    #'          - `NULL`: If no task with such `id` was found.
    update_task = function(id, title = NULL, done = NULL, tags = NULL) {
      task <- self$tasks[[id]]

      if (is.null(task)) {
        return()
      }

      if (!is.null(title)) {
        task$title <- title
      }

      if (!is.null(done)) {
        task$done <- done
      }

      if (!is.null(tags)) {
        task$tags <- tags
      }

      self$tasks[[id]] <- task

      task
    },

    #' Delete Specific Task
    #'
    #' @param id String /// Required.
    #'           Task ID.
    #'
    #' @return Either:
    #'          - Named list: The deleted task, or,
    #'          - `NULL`: if no task with such `id` was found.
    delete_task = function(id) {
      task <- self$tasks[[id]]

      self$tasks[[id]] <- NULL

      task
    }
  )
}

my_todo_list <- TodoList()

# reusable schemas
# schemas are plain r objects: you define once, reuse everywhere.
#
# a schema wrapped in `openapi_schema_ref()` is named: it's placed in the
# document's `components` once and referenced with `$ref` everywhere it is
# used, so the swagger ui shows it as a model of its own.
#
# every `...` argument is a json schema keyword, spelled as in the
# specification: `minLength`, `example`, `enum`, `format`, ...

task_schema <- openapi_schema_ref(
  name = "Task",
  schema = openapi_schema_object(
    properties = list(
      id = openapi_schema_string(description = "unique identifier"),
      title = openapi_schema_string(minLength = 1L, example = "write docs"),
      done = openapi_schema_boolean(default = FALSE),
      tags = openapi_schema_array(
        items = openapi_schema_string(),
        maxItems = 10L
      )
    ),
    required = c("id", "title", "done"),
    description = "A task in the todo list."
  )
)

task_list_schema <- openapi_schema_array(items = task_schema)

new_task_schema <- openapi_schema_ref(
  name = "NewTask",
  schema = openapi_schema_object(
    properties = list(
      title = openapi_schema_string(minLength = 1L),
      done = openapi_schema_boolean(default = FALSE),
      tags = openapi_schema_array(items = openapi_schema_string())
    ),
    required = "title"
  )
)

task_update_schema <- openapi_schema_ref(
  name = "TaskUpdate",
  schema = openapi_schema_object(
    properties = list(
      title = openapi_schema_string(minLength = 1L),
      done = openapi_schema_boolean(),
      tags = openapi_schema_array(items = openapi_schema_string())
    )
  )
)

error_schema <- openapi_schema_ref(
  name = "Error",
  schema = openapi_schema_object(
    properties = list(
      error = openapi_schema_string()
    ),
    required = TRUE
  )
)

app <- Ambiorix$new(port = 3000L)

# enable swagger docs:
# `title`, `version` & `description` fill the openapi `info` object.
#
# with `validate = TRUE` incoming requests are checked against the app's
# documented openapi schemas. handlers only ever see valid input.
app$openapi(
  title = "Tasks API",
  version = "1.0.0",
  description = paste(
    "A small task manager demonstrating ambiorix's openapi support.",
    "All data is held in memory: feel free to create, update, and",
    "delete tasks from the swagger ui."
  ),
  info = list(
    contact = list(
      name = "ambiorix",
      url = "https://ambiorix.dev"
    )
  ),
  servers = "http://localhost:3000",
  tags = c(
    tasks = "create, read, update, and delete tasks",
    admin = "administrative endpoints",
    misc = "everything else"
  ),
  validate = TRUE
)

# landing page: smallest possible docs:
app$get(
  path = "/",
  handler = function(req, res) {
    res$json(
      list(
        message = "welcome to the tasks api. see /docs."
      )
    )
  },
  docs = openapi_docs(
    summary = "landing endpoint",
    tags = "misc",
    responses = openapi_response(
      status = 200L,
      description = "a welcome message"
    )
  )
)

# list tasks:
app$get(
  path = "/tasks",
  handler = function(req, res) {
    x_req_id <- req$HEADERS$`x-request-id`

    if (!is.null(x_req_id)) {
      msg <- sprintf("X-Request-Id: '%s'", x_req_id)
      cat(msg, "\n")
    }

    # when named, an r list is serialized into an object of items.
    # when unnamed, an r list is serialized into an array of items.
    # `task_list_schema` expects an array of objects, so we unname:
    tasks <- my_todo_list$read_all_tasks() |> unname()

    # `done` and `limit` arrive converted to their documented types:
    done <- req$query$done
    if (!is.null(done)) {
      tasks <- Filter(
        f = function(task) {
          identical(task$done, done)
        },
        x = tasks
      )
    }

    limit <- req$query$limit
    if (!is.null(limit) && limit < length(tasks)) {
      tasks <- tasks[seq_len(limit)]
    }

    res$json(tasks)
  },
  docs = openapi_docs(
    summary = "list tasks",
    description = "returns all tasks, optionally filtered and limited.",
    operation_id = "listTasks",
    tags = "tasks",
    parameters = list(
      openapi_param(
        name = "done",
        location = "query",
        description = "only return tasks with this completion status",
        schema = openapi_schema_boolean()
      ),
      openapi_param(
        name = "limit",
        location = "query",
        description = "max number of tasks to return.",
        schema = openapi_schema_integer(minimum = 1L)
      ),
      openapi_param(
        name = "X-Request-Id",
        location = "header",
        description = "optional id echoed in server logs.",
        schema = openapi_schema_string()
      )
    ),
    responses = list(
      openapi_response(
        status = 200L,
        description = "a list of tasks",
        schema = task_list_schema
      )
    )
  )
)

# get one task:
app$get(
  path = "/tasks/:id",
  handler = function(req, res) {
    task <- my_todo_list$read_task(id = req$params$id)

    if (is.null(task)) {
      out <- list(
        error = "task not found"
      )

      res$status <- 404L

      return(
        res$json(out)
      )
    }

    res$json(task)
  },
  docs = openapi_docs(
    summary = "get task by id",
    description = "gets a specific task by id",
    operation_id = "getTask",
    tags = "tasks",
    parameters = list(
      # `:id` is documented automatically but we can override that:
      openapi_param(
        name = "id",
        location = "path",
        description = "the task id",
        schema = openapi_schema_string()
      )
    ),
    responses = list(
      openapi_response(
        status = 200L,
        description = "the task",
        schema = task_schema
      ),
      openapi_response(
        status = 404L,
        description = "task not found",
        schema = error_schema
      )
    )
  )
)

# create a task:
app$post(
  path = "/tasks",
  handler = function(req, res) {
    # since we set `app$openapi(..., validate = TRUE)`, the body has
    # already been validated and stored on `req$payload`:
    # - `title` is there, a non-empty string
    # - `done`, if sent, is a logical
    # - `tags`, if sent, is an array of strings: `["docs"]` arrives as
    #   `I("docs")` and serializes back as `["docs"]`; a bare `"docs"` was
    #   rejected with a 400 before this handler ran
    #
    # schema defaults are documentation, not values: what the client left
    # out is `NULL` here, so the handler supplies the `Task` schema's shape
    body <- req$payload

    if (is.null(body$done)) {
      body$done <- FALSE
    }

    if (is.null(body$tags)) {
      body$tags <- list()
    }

    new_task <- do.call(what = my_todo_list$create_task, args = body)

    res$status <- 201L
    res$json(new_task)
  },
  docs = openapi_docs(
    summary = "create a task",
    operation_id = "createTask",
    tags = "tasks",
    request_body = openapi_request_body(
      schema = new_task_schema,
      description = "the task to create"
    ),
    responses = list(
      openapi_response(
        status = 201L,
        description = "the created task",
        schema = task_schema
      ),
      openapi_response(
        status = 400L,
        description = "invalid input",
        schema = error_schema
      )
    )
  )
)

# update a task:
app$put(
  path = "/tasks/:id",
  handler = function(req, res) {
    body <- req$payload

    body$id <- req$params$id

    updated_task <- do.call(what = my_todo_list$update_task, args = body)

    if (is.null(updated_task)) {
      out <- list(
        error = "task not found"
      )

      res$status <- 404L

      return(
        res$json(out)
      )
    }

    res$json(updated_task)
  },
  docs = openapi_docs(
    summary = "update a task",
    operation_id = "updateTask",
    tags = "tasks",
    parameters = list(
      openapi_param(
        name = "id",
        location = "path",
        schema = openapi_schema_string()
      )
    ),
    request_body = openapi_request_body(
      schema = task_update_schema,
      required = FALSE,
      description = "fields to update; omitted fields are left unchanged."
    ),
    responses = list(
      openapi_response(
        status = 200L,
        description = "the updated task",
        schema = task_schema
      ),
      openapi_response(
        status = 404L,
        description = "task not found",
        schema = error_schema
      )
    )
  )
)

# delete a task:
app$delete(
  path = "/tasks/:id",
  handler = function(req, res) {
    deleted_task <- my_todo_list$delete_task(id = req$params$id)

    if (is.null(deleted_task)) {
      out <- list(
        error = "task not found"
      )

      res$status <- 404L

      return(
        res$json(out)
      )
    }

    res$status <- 204L
    res$send("")
  },
  docs = openapi_docs(
    summary = "delete a task",
    operation_id = "deleteTask",
    tags = "tasks",
    parameters = list(
      openapi_param(
        name = "id",
        location = "path",
        schema = openapi_schema_string()
      )
    ),
    responses = list(
      openapi_response(
        status = 204L,
        description = "task deleted"
      ),
      openapi_response(
        status = 404L,
        description = "task not found",
        schema = error_schema
      ),
      openapi_response(
        status = "default",
        description = "unexpected error",
        schema = error_schema
      )
    )
  )
)

# router: documented routes are rendered with their full path
admin <- Router$new(path = "/admin")

admin$get(
  path = "/stats",
  handler = function(req, res) {
    tasks <- my_todo_list$read_all_tasks()

    total <- length(tasks)

    done <- 0L
    for (task in tasks) {
      if (isTRUE(task$done)) {
        done <- done + 1L
      }
    }

    pending <- total - done

    out <- list(
      total = total,
      done = done,
      pending = pending
    )

    res$json(out)
  },
  docs = openapi_docs(
    summary = "task stats",
    description = "appears in the openapi document as `/admin/stats`.",
    tags = "admin",
    responses = list(
      openapi_response(
        status = 200L,
        description = "count of tasks by status",
        schema = openapi_schema_object(
          properties = list(
            total = openapi_schema_integer(),
            done = openapi_schema_integer(),
            pending = openapi_schema_integer()
          ),
          required = FALSE
        )
      )
    )
  )
)

app$use(admin)

# undocumented route: excluded from the openapi document:
app$get(
  path = "/health",
  handler = function(req, res) {
    out <- list(status = "ok")

    res$json(out)
  }
)

app$start()
