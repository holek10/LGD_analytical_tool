

Progress_current <- setRefClass(
  'Progress_current',
  fields = list(
    .session = 'ANY',
    .id = 'character',
    .min = 'numeric',
    .max = 'numeric',
    .closed = 'logical'
  ),
  methods = list(
    initialize = function(session, min = 0, max = 1) {
      .closed <<- FALSE
      .session <<- session
      .id <<- paste(as.character(as.raw(runif(8, min=0, max=255))), collapse='')
      .min <<- min
      .max <<- max
      
      .session$sendCustomMessage('shiny-progress-open-current', list(id = .id))
    },
    set = function(message = NULL, detail = NULL, value = NULL) {
      if (.closed) {
        # TODO: Warn?
        return()
      }
      
      data <- list(id = .id)
      if (!missing(message))
        data$message <- message
      if (!missing(detail))
        data$detail <- detail
      if (!missing(value)) {
        if (is.null(value) || is.na(value))
          data$value <- NULL
        else {
          data$value <- min(1, max(0, (value - .min) / (.max - .min)))
        }
      }
      
      .session$sendCustomMessage('shiny-progress-update-current', data)
    },
    close = function() {
      if (.closed) {
        # TODO: Warn?
        return()
      }
      
      .session$sendCustomMessage('shiny-progress-close-current',
                                 list(id = .id))
    }
  )
)

#.currentProgress <- new.env()


Progress_historical <- setRefClass(
  'Progress_historical',
  fields = list(
    .session = 'ANY',
    .id = 'character',
    .min = 'numeric',
    .max = 'numeric',
    .closed = 'logical'
  ),
  methods = list(
    initialize = function(session, min = 0, max = 1) {
      .closed <<- FALSE
      .session <<- session
      .id <<- paste(as.character(as.raw(runif(8, min=0, max=255))), collapse='')
      .min <<- min
      .max <<- max
      
      .session$sendCustomMessage('shiny-progress-open-historical', list(id = .id))
    },
    set = function(message = NULL, detail = NULL, value = NULL) {
      if (.closed) {
        # TODO: Warn?
        return()
      }
      
      data <- list(id = .id)
      if (!missing(message))
        data$message <- message
      if (!missing(detail))
        data$detail <- detail
      if (!missing(value)) {
        if (is.null(value) || is.na(value))
          data$value <- NULL
        else {
          data$value <- min(1, max(0, (value - .min) / (.max - .min)))
        }
      }
      
      .session$sendCustomMessage('shiny-progress-update-historical', data)
    },
    close = function() {
      if (.closed) {
        # TODO: Warn?
        return()
      }
      
      .session$sendCustomMessage('shiny-progress-close-historical',
                                 list(id = .id))
    }
  )
)


Progress_summary <- setRefClass(
  'Progress_summary',
  fields = list(
    .session = 'ANY',
    .id = 'character',
    .min = 'numeric',
    .max = 'numeric',
    .closed = 'logical'
  ),
  methods = list(
    initialize = function(session, min = 0, max = 1) {
      .closed <<- FALSE
      .session <<- session
      .id <<- paste(as.character(as.raw(runif(8, min=0, max=255))), collapse='')
      .min <<- min
      .max <<- max
      
      .session$sendCustomMessage('shiny-progress-open-summary', list(id = .id))
    },
    set = function(message = NULL, detail = NULL, value = NULL) {
      if (.closed) {
        # TODO: Warn?
        return()
      }
      
      data <- list(id = .id)
      if (!missing(message))
        data$message <- message
      if (!missing(detail))
        data$detail <- detail
      if (!missing(value)) {
        if (is.null(value) || is.na(value))
          data$value <- NULL
        else {
          data$value <- min(1, max(0, (value - .min) / (.max - .min)))
        }
      }
      
      .session$sendCustomMessage('shiny-progress-update-summary', data)
    },
    close = function() {
      if (.closed) {
        # TODO: Warn?
        return()
      }
      
      .session$sendCustomMessage('shiny-progress-close-summary',
                                 list(id = .id))
    }
  )
)

