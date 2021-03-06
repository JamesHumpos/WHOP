as.data_model <- function(x) {
  UseMethod("as.data_model")
}

#' @keywords internal
#' @export
as.data_model.list <- function(x) {
  
  if(mode(x) != "list"){
    stop("Not a list")
  }
  if(!all(c("columns", "references") %in% (names(x)))) {
    stop("Input must have columns and references")
  }
  
  class(x) <- c("data_model", class(x))
  x
}

#' Check if object is a data model
#'
#' @param x Object to check if it is a data model
#' @keywords internal
#' @export
is.data_model <- function(x) {
  inherits(x, "data_model")
}


#' Coerce a data frame to a data model
#'
#' @keywords internal
#' @export
as.data_model.data.frame <- function(x) {
  
  if(!inherits(x, "data.frame")) stop("Not a data.frame")
  
  if(!all(c("column", "table") %in% names(x)))
  {
    stop("Data frame must have elements named 'table' and 'column'.")
  }
  
  # set key to 0 if NA or add key if NULL:
  if(!is.null(x[["key"]])) {
    x[is.na(x[,"key"]), "key"] <- FALSE
  } else {
    x[,"key"] <- FALSE
  }
  
  # convert logical key markers to numeric (column order in a key)
  # x$table <- factor(x$table, ordered = TRUE)
  # if(max(x$key, na.rm = TRUE) <= 1) {
  #   keys <-
  #     lapply(split(x, x$table), function(t) {
  #       cumsum(t$key) * t$key
  #     })
  #   x$key <- unlist(keys)
  # }
  
  if(is.null(x[["ref"]])) x[["ref"]] <- NA
  
  
  # create references from ref and keys
  ref_table <- dm_create_references(x)
  
  table_attrs <- attr(x, "tables")
  if(is.null(table_attrs)) {
    table_attrs <-
      data.frame(
        table = unique(x[["table"]]),
        segment = NA,
        display = NA,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
  }
  attr(x, "tables") <- NULL
  ret <- list(
    tables = table_attrs,
    columns = x,
    references = ref_table
  )
  as.data_model(ret)
}

#' Read YAML
#'
#' Reads a file in YAML format and returns a data model object.
#'
#' @details YAML description should include table names (first level),
#' columns (second level) and column attributes (third level).
#' Expected (but not required) column attributes are
#'   \code{key} (Yes|No),
#'   \code{ref} (Name of referenced table),
#'   \code{comment} (column description).
#'
#' @param file A file in YAML format
#' @param text A YAML formated character string
#' @examples
#' dm <-
#'   dm_read_yaml(text = "
#'
#'     Person:
#'       Person ID: {key: yes}
#'       Name:
#'       E-mail:
#'       Street:
#'       Street number:
#'       City:
#'       ZIP:
#'
#'     Order:
#'       Order ID: {key: yes}
#'       Customer: {ref: Person}
#'       Sales person: {ref: Person}
#'       Order date:
#'       Requested ship date:
#'       Status:
#'
#'     Order Line:
#'       Order ID: {key: yes, ref: Order}
#'       Line number: {key: yes}
#'       Order item: {ref: Item}
#'       Quantity:
#'       Price:
#'
#'     Item:
#'       Item ID: {key: yes}
#'       Item Name:
#'       Description:
#'   ")
#' @export
dm_read_yaml <- function(file = NULL, text = NULL) {
  
  if( !requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  
  if(missing(text)) {
    if(!missing(file)) {
      if(!file.exists(file)) stop("File does not exist.")
      dm <- yaml::yaml.load_file(file)
    } else {
      stop("A file or text needed.")
    }
  } else {
    dm <- yaml::yaml.load(text)
  }
  if(is.null(dm)) {
    return(NULL)
  }
  
  col_table <- dm_list2coltable(dm)
  return(as.data_model(col_table))
}


#' List to column table
#'
#' Convert a 3 level named list to a data frame with column info
#'
#' @details The funcion is used when creating data model object
#'   from list provided by yaml parser.
#' @param x a named list
#' @export
#' @keywords internal
dm_list2coltable <- function(x) {
  
  if(!is.list(x)) {
    stop("Input must be a list.")
  }
  
  if(is.null(names(x))) {
    # parsed yaml with sequences
    x_tables <- x[sapply(x, function(x) !is.null(x[["table"]]))]
    
    table_names <- sapply(x_tables, function(tab) tab[["table"]])
    columns <- lapply(x_tables, function(tab) {
      tab_name <- tab[["table"]]
      if(!is.null(tab_name)) {
        cols <- tab[["columns"]]
      }
    })
    names(columns) <- table_names
    
    column_names <- lapply(columns, names)
    column_attributes <- unique( unlist( lapply(columns, sapply, names)))
    
  } else {
    # Named list (parsed yaml with maps)
    columns <- x
    table_names <- names(columns)
    column_names <- lapply(columns, names)
    column_attributes <- unique( unlist( lapply(columns, sapply, names)))
  }
  
  
  table_list <-
    lapply(table_names, function(tab_name) {
      if(is.null(column_names[[tab_name]])) {
        column_names[[tab_name]] <- NA
      }
      tab <- data.frame(
        table = tab_name,
        column = column_names[tab_name],
        stringsAsFactors = FALSE
      )
      names(tab) <- c("table", "column")
      
      for(a in column_attributes) {
        attr_value <-
          unlist(
            sapply(column_names[[tab_name]], function(cname) {
              if(is.list(columns[[tab_name]][[cname]]))
                value <- columns[[tab_name]][[cname]][[a]]
              else
                value <- NA
              ifelse(is.null(value), NA, value)
            })
          )
        tab[[a]] <- attr_value
      }
      tab
    })
  
  ret <- do.call(rbind, table_list)
  
  table_attrs <- dm_get_table_attrs(x)
  if(!is.null(table_attrs) && is.null(table_attrs$segment))
    table_attrs$segment <- NA
  attr(ret, "tables") <- table_attrs
  
  ret
}

dm_get_table_attrs <- function(x) {
  
  x_tables <- x[sapply(x, function(x) !is.null(x[["table"]]))]
  table_names <- sapply(x_tables, function(tab) tab[["table"]])
  table_attrs <- unique(unlist(lapply(x_tables, names)))
  table_attrs <- table_attrs[!table_attrs %in% c("columns", "table")]
  names(x_tables) <- table_names
  
  table_attrs <-
    lapply(table_names, function(tab) {
      ret <-
        data.frame(
          table = tab,
          stringsAsFactors = FALSE
        )
      for(aname in table_attrs) {
        tab_attr <- x_tables[[tab]][[aname]]
        if(is.null(tab_attr)) {
          tab_attr <- NA
        }
        ret[[aname]] <- tab_attr
      }
      ret
    })
  
  do.call(rbind, table_attrs)
}


#' Create reference info
#'
#' Creates references (foreign keys) based on reference table names in
#' column info table.
#'
#' @param col_table A data frame with table columns
#' @details The function is used when creating data model object.
#'   \code{col_table} must have at least
#'     \code{table},
#'     \code{column} and
#'     \code{ref} elements.
#'   When referencing to tables with compound primary keys
#'   additional \code{ref_col} with primary key columns must be provided.
#' @export
#' @keywords internal
dm_create_references <- function(col_table) {
  
  if(!inherits(col_table, "data.frame")) stop("Input must be a data frame.")
  
  if(!all(c("table", "column") %in% names(col_table))) {
    stop("Column info table must have table, column and ref variables.")
  }
  if(!"ref" %in% names(col_table)) {
    return(NULL)
  }
  if(all(is.na(col_table[,"ref"]))) {
    return(NULL)
  }
  
  
  if(is.null(col_table[["ref_col"]])) {
    col_table[["ref_col"]] <- NA
  }
  ref_table <- col_table[
    !is.na(col_table[["ref"]]),  # take only rows with reference
    c("table", "column", "ref", "ref_col")]
  col_table[is.na(col_table$key), "key"] <- FALSE
  
  ref_col <-
    with(ref_table,
         ifelse(is.na(ref_col),
                sapply(ref_table$ref, function(x)
                  col_table[col_table$table == x & col_table$key, "column"][1]
                ),
                ref_col
         )
    )
  ref_table[["ref_col"]] <- ref_col
  
  # number of columns in primary key
  num_col = sapply(ref_table$ref, function(x)
    length(col_table[col_table$table == x & col_table$key, ][["column"]])
  )
  num_col[num_col == 0L] <- 1L
  
  key_col_num = {
    
    # create column index number
    rle1 <- rle(num_col)
    if(lengths(rle1)[1] > 0) {
      col_list <- sapply(1:lengths(rle1)[1], function(i) {
        rep(1 : rle1$values[i], rle1$lengths[i] / rle1$values[i])
      })
      col_list[lengths(col_list) == 0] <- 1
      unlist(col_list)
    } else {
      NA
    }
  }
  
  dim(key_col_num) <- NULL
  if(nrow(ref_table) == length(key_col_num)) {
    ref_table$ref_id <- cumsum(key_col_num == 1)
    ref_table$ref_col_num <- key_col_num
  } else {
    ref_table$ref_col_num <- 1
    ref_table$ref_id <- cumsum(ref_table$ref_col_num)
  }
  ref_table
}


#' Create data model object from R data frames
#'
#' Uses data frame column names to create a data model diagram
#'
#' @param ... Data frames or one list of data frames
#' @export
dm_from_data_frames <- function(...) {
  
  df_list <- list(...)
  if(length(df_list) == 1 && inherits(df_list[[1]], "list")) {
    df_list <- df_list[[1]]
  } else {
    if(length(names(df_list)) < length(df_list)) {
      names(df_list) <- as.list(match.call( expand.dots = TRUE)[-1])
    }
  }
  tables <- df_list
  names(tables) <- make.names(names(tables))
  dfdm <-
    do.call(rbind,
            lapply(names(tables), function(table_name) {
              t1 <- tables[[table_name]]
              columns <- data.frame(
                column = names(t1),
                type = sapply(t1[0, , drop = FALSE], function(x) paste(class(x), collapse = ", ")),
                stringsAsFactors = FALSE)
              columns$table <- table_name
              columns
            })
    )
  as.data_model(dfdm)
  
}

#' Add reference
#'
#' Adds reference to existing data model object
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @param ref Referenced table name
#' @param ref_col Referenced column(s) name
#' @return New data model object
#' @export
dm_add_reference_ <- function(dm, table, column, ref = NULL, ref_col = NULL) {
  ref_df <-
    data.frame(
      table = table,
      column = column,
      ref = ref,
      ref_col = ref_col,
      ref_id = ifelse(is.null(dm$references), 1, max(dm$references$ref_id) + 1),
      ref_col_num = 1:(length(ref_col)),
      
      stringsAsFactors = FALSE
    )
  dm$references <- rbind(dm$references, ref_df)
  dm$columns$ref[dm$columns$table == table & dm$columns$column %in% column] <- ref
  dm
}

#' Add references
#'
#' Adds references defined with logical expressions from data frames
#'   in format table1$column1 == table2$column2
#'
#' @param dm Data model object
#' @param ... Logical expressions in format table1$column1 == table2$column2
#' @export
dm_add_references <- function(dm, ...)
{
  ref_list <- substitute(list(...))
  
  if(is.null(dm$columns$ref)) dm$columns$ref <- NA
  if(is.null(dm$columns$ref_col)) dm$columns$ref_col <- NA
  if(is.null(dm$columns$key)) dm$columns$key <- FALSE
  
  for(ref in as.list(ref_list[-1])) {
    ref <- as.list(ref)
    if(
      as.character(ref[1]) != "`==`" ||
      length(ref) != 3 || length(ref[[2]]) != 3 || length(ref[[3]]) != 3) {
      stop("Define references with logical expressions:
           dataframe1$column1 == dataframe2$column2, ...",
           call. = FALSE)
    }
    toChar <- function(ref, i, j) as.character(ref[[i]][[j]])
    
    table_name  = as.character(ref[[2]][[2]])
    column_name = as.character(ref[[2]][[3]])
    ref_table   = as.character(ref[[3]][[2]])
    ref_col     = as.character(ref[[3]][[3]])
    
    dm_row <- with(dm$columns, table == table_name & column == column_name)
    dm$columns[dm_row, "ref"] <- ref_table
    dm$columns[dm_row, "ref_col"] <- ref_col
    
    dm_key_row <- dm$columns$table == ref_table & dm$columns$column == ref_col
    dm$columns[dm_key_row, "key"] <- TRUE
  }
  
  ref_table <- dm_create_references(dm$columns)
  dm$references <- ref_table
  dm
}


#' Set key
#'
#' Set column as a primary key
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @export
dm_set_key <- function(dm, table, column) {
  update_cols <- dm$columns$table == table & dm$columns$column %in% column
  if(!any(update_cols)) {
    stop("Column not found.")
  }
  dm$columns$key[update_cols] <- seq_along(column)
  dm
}

#' Set column attribute
#'
#' Set column attribute value
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @param attr Column attribute name
#' @param value New value
#' @export
#' @keywords internal
dm_set_col_attr <- function(dm, table, column, attr, value) {
  update_cols <- dm$columns$table == table & dm$columns$column == column
  if(!any(update_cols)) {
    stop("Column not found.")
  }
  dm$columns[update_cols, attr] <- value
  dm
}


#' Reverse engineer query
#'
#' Returns a string with SQL query to reverse engineer a database
#'
#' @param rdbms Which database ("postgres" or "sqlserver")
#' @return A character string with sql query
#' @export
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' # dvdrental sample database: http://www.postgresqltutorial.com/postgresql-sample-database
#' con <- dbConnect(dbDriver("PostgreSQL"), dbname="dvdrental", user ="postgres")
#' sQuery <- dm_re_query("postgres")
#' dm_dvdrental <- dbGetQuery(con, sQuery)
#' dbDisconnect(con)
#' }
dm_re_query <- function(rdbms) {
  sql_script <- sprintf("sql/%s.sql", rdbms)
  file_name <- system.file(sql_script, package ="datamodelr")
  if( !file.exists(file_name) ) {
    stop("This rdbs not supported")
  }
  sQuery <- paste(readLines(file_name), collapse = "\n")
  sQuery
}


#' Set table segment
#'
#' Change tables' segment name in a data model
#'
#' @param dm A data model object
#' @param table_segments A named list of vectors with segments as element names
#'   and tables as values in vectors
#' @export
dm_set_segment <- function(dm, table_segments) {
  
  if(!is.data_model(dm))
    stop("Not a data model object.")
  for(s in names(table_segments)) {
    table_names <- table_segments[[s]]
    dm$tables$segment[dm$tables$table %in% table_names ] <- s
  }
  dm
}

#' Set table display
#'
#' Change tables' display in a data model
#'
#' @param dm A data model object
#' @param display A named list of vectors with display as element names
#'   and tables as values in vectors
#' @export
dm_set_display <- function(dm, display) {
  
  if(!is.data_model(dm))
    stop("Not a data model object.")
  for(s in names(display)) {
    table_names <- display[[s]]
    dm$tables$display[dm$tables$table %in% table_names ] <- s
  }
  dm
}

#' Print data model graph
#'
#' @param x data model object.
#' @param ... further arguments passed to or from other methods.
#' @export
print.data_model <- function(x, ...) {
  cat("Data model object:\n")
  tables <- paste(utils::head(x$tables$table, 4), collapse = ", ")
  if(length(x$tables$table) > 4) {
    tables <- paste(tables, "...")
  }
  cat(" ", nrow(x$tables), "tables: ", tables,"\n")
  cat(" ", nrow(x$columns), "columns\n")
  cat(" ", length(unique(x$columns[x$columns[["key"]] != 0,"table"])), "primary keys\n")
  cat(" ", ifelse(is.null(x$references), "no", nrow(unique(x$references))),
      "references\n")
}


html_tag <- function(x, tag, ident = 0, nl = TRUE, atrs = NULL, collapse = "") {
  if(length(x) > 1 && !is.null(collapse)) {
    x <- paste(x, collapse = collapse)
  }
  space <- paste(rep("  ", ident), collapse = "")
  atrs <- paste(sprintf('%s="%s"', names(atrs), atrs), collapse = " ")
  if(nchar(atrs) > 0) atrs <- paste0(" ", atrs)
  htext <-
    if(nl) {
      sprintf("%s<%s%s>\n%s%s</%s>\n", space, tag, atrs, x, space, tag)
    } else {
      sprintf("%s<%s%s>%s</%s>\n", space, tag, atrs, x, tag)
    }
  paste(htext, collapse = "")
}

html_table <- function(x, ...) html_tag(x, tag = "TABLE", ident = 1, ...)
html_tr <- function(x, ...)    html_tag(x, tag = "TR", ident = 2, ...)
html_td <- function(x, ...)    html_tag(x, tag = "TD", ident = 3, nl = FALSE, ...)
html_font <- function(x, ...)    html_tag(x, tag = "FONT", ident = 0, nl = FALSE, ...)

#' Data frame to html table
#'
#' Used to create graphwiz dot HTML table labels
#'
#' @param x A data frame
#' @param attr_table A named list with table attributes
#' @param attr_header A named list with cell attributes in header cell
#' @param attr_td A function with parameters column, current row, value and
#'   returns a named list with current cell attributes
#' @param cols A named vector of columns to include in a table
#' @param trans Value transformation funcion
#' @keywords internal
#' @export
to_html_table <- function (x,
                           title = "Table",
                           attr_table,
                           attr_header,
                           attr_font,
                           attr_td = NULL,
                           trans = NULL,
                           cols = names(x)) {
  
  html_table(atrs = attr_table, c(
    # header
    html_tr(
      html_td(
        html_font(title, atrs = attr_font),
        atrs = attr_header,
        collapse = NULL
      )
    ),
    # rows
    sapply(seq_len(nrow(x)), function(r)
      html_tr(c(
        # cells
        sapply(cols, function(col_name) {
          value <- x[r, col_name]
          if(!is.null(trans)) value <- trans(col_name, x[r,], value)
          html_td(value, if(is.null(attr_td)) NULL else attr_td(col_name, x[r,], value))
        })
      ))
    )
  ))
}


#' Create a label
#'
#' Create a label for dot HTML shape
#'
#' @param x A data frame with column info
#' @param title A node title
#' @param palette_id Which color palette should be used (default)
#' @param col_attr Column atributes to display.
#'   Only column name (\code{column}) is included by default.
#' @param columnArrows Arrows between columns instead of tables (default: FALSE)
#' @keywords internal
#' @references See \url{http://www.graphviz.org/content/node-shapes}
#' @export
dot_html_label <- function(x, title, palette_id = "default", col_attr = c("column"),
                           columnArrows = FALSE ) {
  cols <- c("ref", col_attr)
  if(is.null(palette_id)) {
    palette_id <- "default"
  }
  
  border = ifelse(is.null(dm_color(palette_id, "line_color")), 0, 1)
  
  attr_table <- list(
    ALIGN="LEFT", BORDER=border, CELLBORDER=0, CELLSPACING=0
  )
  if(!is.null(dm_color(palette_id, "line_color"))) {
    attr_table[["COLOR"]] <- dm_color(palette_id, "line_color")
  }
  attr_header <- list(
    COLSPAN=length(cols) - columnArrows, BGCOLOR=dm_color(palette_id, "header_bgcolor"), BORDER=0
  )
  attr_font <- list()
  attr_font <- list(COLOR = dm_color(palette_id, "header_font"))
  
  attr_td <- function(col_name, row_values, value) {
    ret <- list(ALIGN="LEFT", BGCOLOR = dm_color(palette_id, "bgcolor"))
    if(col_name == "column" && columnArrows) {
      key <- row_values[["key"]];
      reference <- row_values[["ref"]];
      if(!is.na(reference) || key) {
        ret$PORT = row_values[["column"]];
      }
    }
    ret;
  }
  
  # value presentation transformation
  trans <- function(col_name, row_values, value) {
    if(col_name == "ref") {
      value <- ifelse(is.na(value), "", "~")
      if(columnArrows) {
        value <- NULL;
      }
    }
    if(col_name == "column" && row_values[["key"]]) {
      value <- sprintf("<U>%s</U>", value)
    }
    if(!is.null(value) && is.na(value)) {
      value = "";
    }
    return(value);
  }
  
  ret <- to_html_table(x, title = title,
                       attr_table = attr_table,
                       attr_header = attr_header,
                       attr_font = attr_font,
                       attr_td = attr_td,
                       cols = cols,
                       trans = trans)
  
  ret <- sprintf("<%s>", trimws(ret))
  ret
}


dm_create_graph_list <- function(dm, view_type = "all",
                                 focus = NULL, col_attr = "column",
                                 columnArrows = FALSE) {
  
  if(!is.data_model(dm)) stop("Input must be a data model object.")
  
  # hidden tables
  
  if(!is.null(focus) && is.list(focus)) {
    if(!is.null(focus[["tables"]])) {
      dm$tables <- dm$tables[dm$tables$table %in% focus$tables,  ]
      dm$columns <- dm$columns[dm$columns$table %in% focus$tables,  ]
      if(is.null(focus[["external_ref"]]) || !focus[["external_ref"]]) {
        dm$references <- dm$references[
          dm$references$table %in% focus$tables &
            dm$references$ref %in% focus$tables, ]
      }
    }
  } else {
    # hide tables with display == "hide" attribute
    if(is.null(dm$tables$display)) dm$tables$display <- NA
    dm$tables$display[is.na(dm$tables$display)] <- "show"
    hidden_tables <- dm$tables[dm$tables$display == "hide", "table"]
    if(!is.null(hidden_tables)) {
      dm$tables <- dm$tables[!dm$tables$table %in% hidden_tables,  ]
      dm$columns <- dm$columns[!dm$columns$table %in% hidden_tables,  ]
      dm$references <- dm$references[
        !dm$references$table %in% hidden_tables &
          !dm$references$ref %in% hidden_tables, ]
    }
  }
  
  
  # remove hidden columns
  # dm$columns <-
  #  dm$columns[is.na(dm$columns$display) | dm$columns$display != "hide", ]
  
  tables <- split(dm$columns, dm$columns$table)
  
  switch( view_type,
          all = {},
          
          keys_only = {
            tables <- lapply(tables, function(tab)
              tab[tab[["key"]] > 0 | !is.na(tab[,"ref"]), ])
          },
          
          title_only = {
            tables <- lapply(tables, function(tab)
              tab[0L,])
          })
  
  g_labels <-
    sapply(names(tables), function(x) {
      dot_html_label(
        tables[[x]],
        title = x,
        palette_id = dm$tables[dm$tables$table == x, "display"],
        col_attr = col_attr,
        columnArrows = columnArrows)
    })
  
  nodes <-
    data.frame(
      nodes = names(tables),
      label = g_labels,
      shape = "plaintext",
      type = "upper",
      segment = dm$tables[order(dm$tables$table), "segment"],
      
      stringsAsFactors = FALSE
    )
  
  
  if(!is.null(dm$references)) {
    edges <-
      with(dm$references[dm$references$ref_col_num == 1,],
           data.frame(
             from = table,
             to = ref,
             fromCol = column,
             toCol = ref_col,
             stringsAsFactors = FALSE))
  } else {
    edges <- NULL
  }
  
  ret <-
    list(nodes = nodes, edges = edges)
  
  ret
}

#' Create a graph object from data model object
#'
#' @param dm A data model object
#' @param rankdir Graph attribute for direction (eg. 'BT' = bottom --> top)
#' @param graph_name A graph name
#' @param graph_attrs Additional graph attributes
#' @param node_attrs Additional node attributes
#' @param edge_attrs Additional edge attributes
#' @param view_type Can be "all" (by default), "keys_only" or "title_only".
#'   It defines the level of details for the table rendering
#'   (all columns, only primary and foreign keys or no columns)
#' @param focus A list of parameters for rendering (table filter)
#' @param col_attr Column atributes to display.
#'   Only column name (\code{column}) is included by default.
#' @param columnArrows Edges from column to column (default: FALSE)
#' @export
dm_create_graph <- function(dm, rankdir = "BT", graph_name = "Data Model",
                            graph_attrs = "",
                            node_attrs = "",
                            edge_attrs = "",
                            view_type = "all", focus = NULL,
                            col_attr = "column",
                            columnArrows = FALSE) {
  
  if(!is.data_model(dm)) stop("Input must be a data model object.")
  
  
  if(!all(col_attr %in% names(dm$columns) )) {
    stop("Not all col_attr in data model column attributes.")
  }
  g_list <-
    dm_create_graph_list(dm = dm, view_type = view_type,
                         focus = focus, col_attr = col_attr,
                         columnArrows = columnArrows)
  if(length(g_list$nodes$nodes) == 0) {
    warning("The number of tables to render is 0.")
  }
  graph <-
    list(
      graph_attrs = sprintf('rankdir=%s tooltip="%s" %s', rankdir, graph_name, graph_attrs),
      node_attrs = sprintf('margin=0 fontcolor = "#444444" %s', node_attrs),
      nodes_df = g_list$nodes,
      edges_df = g_list$edges,
      edge_attrs = c('color = "#555555"',"arrowsize = 1", edge_attrs)
    )
  class(graph) <- c("datamodelr_graph", class(graph))
  
  # re-create dot code for data model
  # (DiagrammeR does not support yet the HTML labels and clusters (v.0.8))
  graph$dot_code <- dot_graph(graph, columnArrows)
  
  graph
  
}


#' Render graph
#'
#' Using DiagrammeR to render datamodelr graph object
#'
#' @param graph	a graph object
#' @param width	an optional parameter for specifying the width of the resulting
#' graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @export
dm_render_graph <- function (graph, width = NULL, height = NULL) {
  
  if( !requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  
  if(is.null(graph$dot_code)) {
    graph$dot_code <- dot_graph(graph)
  }
  
  DiagrammeR::grViz(graph$dot_code, allow_subst = FALSE, width, height)
}


dot_graph <- function(graph, columnArrows = FALSE) {
  
  graph_type <- "digraph"
  
  dot_attr <- paste0(
    sprintf("graph [%s]\n\n", paste(graph$graph_attrs, collapse = ", ")),
    sprintf("node [%s]\n\n", paste(graph$node_attrs, collapse = ", ")),
    sprintf("edge [%s]\n\n", paste(graph$edge_attrs, collapse = ", "))
  )
  segments <- unique(graph$nodes_df$segment)
  segments <- segments[!is.na(segments)]
  segments <- stats::setNames(1:(length(segments)), segments)
  
  dot_nodes <- sapply(seq_len(nrow(graph$nodes_df)), function(n) {
    node <- graph$nodes_df[n,]
    dot_node <- sprintf("  '%s' [label = %s, shape = '%s'] \n", node$nodes, node$label, node$shape)
    if(!is.na(node[["segment"]])) {
      dot_node <- sprintf("subgraph cluster_%s {\nlabel='%s'\ncolor=\"#DDDDDD\"\n%s\n}\n",
                          segments[node[["segment"]]],
                          node[["segment"]],
                          dot_node
      )
      
    }
    dot_node
  })
  
  dot_seg_nodes <- paste(dot_nodes, collapse = "\n")
  dot_edges <- "";
  if(columnArrows) {
    dot_edges <- paste(
      sprintf("'%s':'%s'->'%s':'%s'",
              graph$edges_df$from,
              graph$edges_df$fromCol,
              graph$edges_df$to,
              graph$edges_df$toCol
      ),
      collapse = "\n"
    )
  } else {
    dot_edges <- paste(
      sprintf("'%s'->'%s'", graph$edges_df$from, graph$edges_df$to),
      collapse = "\n"
    )
  }
  ret <- sprintf("#data_model\n%s {\n%s\n%s\n%s\n}",
                 graph_type,
                 dot_attr,
                 dot_seg_nodes,
                 dot_edges)
  ret
}

#' Datamodel color schema
#'
#' Manage color schema for data model diagrams
#'
#' @param line_color Rectangle color
#' @param header_bgcolor Table header background color
#' @param header_font Table header font color
#' @param bgcolor Table background color
#' @export
#' @examples
#' col_scheme <-
#'   dm_color_scheme(
#'     dm_palette(
#'       line_color = "#787878",
#'       header_bgcolor = "#A5A5A5",
#'       header_font = "#FFFFFF",
#'       bgcolor = "#E4E4E4"
#'     ),
#'     dm_palette(
#'       line_color = "#41719C",
#'       header_bgcolor = "#5B9BD5",
#'       header_font = "#FFFFFF",
#'       bgcolor = "#D6E1F1"
#'     ),
#'     dm_palette(
#'       line_color = "#BC8C00",
#'       header_bgcolor = "#FFC000",
#'       header_font = "#FFFFFF",
#'       bgcolor = "#FFEAD0"
#'     )
#'   )
dm_palette <- function(line_color = NULL, header_bgcolor, header_font, bgcolor) {
  list(
    line_color = line_color,
    header_bgcolor = header_bgcolor,
    header_font = header_font,
    bgcolor = bgcolor
  )
}

#' @param ... Palettes for color schema
#' @export
#' @rdname dm_palette
#' @keywords internal
dm_color_scheme <- function(...) {
  list(...)
}


#' @param color_scheme New colors created with dm_color_scheme
#' @export
#' @rdname dm_palette
dm_add_colors <- function(color_scheme)
{
  old_cs <- dm_get_color_scheme()
  if(any(names(color_scheme) %in% names(old_cs))) {
    old_cs[names(color_scheme)] <- NULL
  }
  dm_set_color_scheme(c(old_cs, color_scheme))
}

#' @export
#' @rdname dm_palette
dm_get_color_scheme <- function() {
  getOption("datamodelr.scheme")
}

#' @export
#' @rdname dm_palette
dm_set_color_scheme <- function(color_scheme) {
  options(datamodelr.scheme = color_scheme)
}

dm_color <- function(palette_id, what) {
  color_scheme <- dm_get_color_scheme()
  if(is.null(color_scheme[[palette_id]])) {
    palette_id <- "default"
  }
  color_scheme[[palette_id]][[what]]
}

dm_set_color_scheme(
  dm_color_scheme(
    default = dm_palette(
      line_color = "#555555",
      header_bgcolor = "#EFEBDD",
      header_font = "#000000",
      bgcolor = "#FFFFFF"
    ),
    accent1nb = dm_palette(
      header_bgcolor = "#5B9BD5",
      header_font = "#FFFFFF",
      bgcolor = "#D6E1F1"
    ),
    accent2nb = dm_palette(
      header_bgcolor = "#ED7D31",
      header_font = "#FFFFFF",
      bgcolor = "#F9DBD2"
    ),
    accent3nb = dm_palette(
      header_bgcolor = "#FFC000",
      header_font = "#FFFFFF",
      bgcolor = "#FFEAD0"
    ),
    accent4nb = dm_palette(
      header_bgcolor = "#70AD47",
      header_font = "#FFFFFF",
      bgcolor = "#D9E6D4"
    ),
    accent5nb = dm_palette(
      header_bgcolor = "#4472C4",
      header_font = "#FFFFFF",
      bgcolor = "#D4D9EC"
    ),
    accent6nb = dm_palette(
      header_bgcolor = "#A5A5A5",
      header_font = "#FFFFFF",
      bgcolor = "#E4E4E4"
    ),
    accent7nb = dm_palette(
      header_bgcolor = "#787878",
      header_font = "#FFFFFF",
      bgcolor = "#D8D8D8"
    ),
    accent1 = dm_palette(
      line_color = "#41719C",
      header_bgcolor = "#5B9BD5",
      header_font = "#FFFFFF",
      bgcolor = "#D6E1F1"
    ),
    accent2 = dm_palette(
      line_color = "#AE5A21",
      header_bgcolor = "#ED7D31",
      header_font = "#FFFFFF",
      bgcolor = "#F9DBD2"
    ),
    accent3 = dm_palette(
      line_color = "#BC8C00",
      header_bgcolor = "#FFC000",
      header_font = "#FFFFFF",
      bgcolor = "#FFEAD0"
    ),
    accent4 = dm_palette(
      line_color = "#507E32",
      header_bgcolor = "#70AD47",
      header_font = "#FFFFFF",
      bgcolor = "#D9E6D4"
    ),
    accent5 = dm_palette(
      line_color = "#2F528F",
      header_bgcolor = "#4472C4",
      header_font = "#FFFFFF",
      bgcolor = "#D4D9EC"
    ),
    accent6 = dm_palette(
      line_color = "#787878",
      header_bgcolor = "#A5A5A5",
      header_font = "#FFFFFF",
      bgcolor = "#E4E4E4"
    ),
    accent7 = dm_palette(
      line_color = "#000000",
      header_bgcolor = "#787878",
      header_font = "#FFFFFF",
      bgcolor = "#D8D8D8"
    )
  )
)




#' Get graph SVG
#'
#' Convert diagram graph object to SVG format
#'
#' @param graph a graph object
#' @return character in SVG format
#' @export
dm_get_graph_svg <- function(graph) {
  
  if (!inherits(graph, "datamodelr_graph"))
    "graph must be a datamodelr graph object"
  
  if (!requireNamespace("V8"))
    stop("V8 is required to export.", call. = FALSE)
  stopifnot(utils::packageVersion("V8") >= "0.10")
  
  gv <- dm_render_graph(graph)
  ct <- V8::new_context("window")
  invisible(ct$source(system.file("htmlwidgets/lib/viz/viz.js",
                                  package = "DiagrammeR")))
  invisible(
    ct$call("Viz", gv$x$diagram, "svg", gv$x$config$engine, gv$x$config$options)
  )
  
}


#' Export graph to file
#'
#' Export data model graph object object to PNG, PDF, PS or SVG file.
#'
#' @param graph a graph object
#' @param file_name file name
#' @param file_type file type (if not provided, file name extension is used)
#' @param width width
#' @param height height
#' @export
dm_export_graph <- function(graph, file_name = NULL, file_type = NULL, width = NULL, height = NULL) {
  
  if(is.null(file_name)) {
    file_name <- format(Sys.time(), "dm_%Y%m%d_%H%M%S")
  }
  if(is.null(file_type) && grepl("\\.", file_name)) {
    file_type <- gsub(".*\\.([A-Za-z])", "\\1", file_name)
  }
  
  if(is.null(file_type)) {
    stop("File type not defined")
  }
  
  if (!("rsvg" %in% rownames(utils::installed.packages()))) {
    stop("To use this function to produce an image file, please install the `rsvg` package.")
  }
  
  render_functions <- list(
    png = rsvg::rsvg_png,
    pdf = rsvg::rsvg_pdf,
    svg = rsvg::rsvg_svg,
    ps = rsvg::rsvg_ps
  )
  
  
  if(!tolower(file_type) %in% names(render_functions) ) {
    stop("File type can be only pdf, png or ps.")
  }
  
  render_function <- render_functions[[tolower(file_type)]]
  
  render_function(
    charToRaw(
      dm_get_graph_svg(graph)
    ),
    file = file_name,
    width = width,
    height = height
  )
  
}

#' Print data model graph
#'
#' @param x data model object
#' @param ... parameter passed to \link{dm_render_graph}
#' @export
print.datamodelr_graph <- function(x, ...) {
  w <- dm_render_graph(x)
  print(w, ...)
}
