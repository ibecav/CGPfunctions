#' Produce CHAID results tables from a partykit CHAID model
#' 
#' @param chaidobject An object of type `constparty` or `party` which
#'   was produced by `CHAID::chaid` see simple example below.
#' @return A tibble containing the results.
#' 
#' @import dplyr
#' @import partykit
#'
#' @importFrom purrr map_dfr
#' @importFrom stringr str_replace_all str_split str_trim
#' @importFrom dplyr as_tibble
#' @importFrom utils getFromNamespace
#' @importFrom tidyr pivot_wider separate
#' @importFrom grid depth
#' @importFrom stats complete.cases formula 
#'
#' @author Chuck Powell
#'
#' @examples
#' library(CGPfunctions)
#' chaid_table(chaidUS)
#' 
#' @export
#' 

chaid_table <- function(chaidobject) {
  # circumventing the fact that partykit doesn't export a function I need
  .list.rules.party = getFromNamespace(".list.rules.party", 
                                       "partykit")

  # extract the formula from the object
  model.formula <- formula(chaidobject)

  # extract the name of the outcome variable
  # all.vars(model.formula)[[1]]
  outcome.variable <- all.vars(model.formula)[[1]]
  
  # get all the nodeids as a vector
  all_nodes <- partykit::nodeids(chaidobject)
  
  # get the terminal nodes as a vector
  terminal_nodes <- partykit::nodeids(chaidobject, 
                                      terminal = TRUE)
  
  # get the split points or inner nodes or non-terminal nodes as a vector
  inner_nodes <- all_nodes[!(all_nodes %in% nodeids(chaidobject, 
                                                    terminal = TRUE))]
  
  # partykit:::.list.rules.party extracts the split rule that generates
  # the prediction for a single node we've pulled it in above with
  # getFromNamespace(".list.rules.party", "partykit")
  node_rules <- .list.rules.party(chaidobject, 
                                  1:length(chaidobject))
  
  # create an empty tibble to start populating the node table
  node_table <- tibble::tibble()
  
  # rejoin the oucome variable to the rest of the data frame
  # since chaid separated them
  original_df <- cbind(chaidobject$data, 
                       outcome = chaidobject$fitted$`(response)`)
  
  # a simple for loop to iterate through all the nodes
  # logic for the top node #1 is slightly different since
  # it has no split rule since it was never split
  # Complete cases is probably unnecessary
  # .drop = FLASE keeps us from losing counts to NA if there are no cases
  # eval(parse(text = node_rules[[i]])) uses valid filter conditions
  for (i in all_nodes) {
    if (i == 1) {
      xxx <- original_df %>%
        filter(complete.cases(.)) %>%
        group_by(outcome) %>%
        summarise(N = n()) %>%
        tidyr::pivot_wider(
          names_from = outcome,
          values_from = N
        )
      xxx$splitrule <- NA
    } else {
      xxx <- original_df %>%
        filter(complete.cases(.)) %>%
        group_by(outcome, .drop = FALSE) %>%
        filter(eval(parse(text = node_rules[[i]]))) %>%
        summarise(N = n()) %>%
        pivot_wider(
          names_from = outcome,
          values_from = N
        )
      xxx$splitrule <- gsub("\"",
                            "'",
                            node_rules[i],
                            fixed = TRUE
      )
    }
    xxx$nodeID <- i
    node_table <- rbind(node_table, xxx)
  }
  
  # technically may be more than 2 must test
  outcome.levels <- nlevels(original_df$outcome)
  
  node_table <- 
    tidyr::separate(
      data = node_table,
      col = splitrule,
      sep = "&",
      into = paste0("split", 
                    1:depth(chaidobject)),
      remove = FALSE,
      fill = "right") %>%
    mutate(NodeN = rowSums(.[1:outcome.levels])) %>%
    select(nodeID, 
           NodeN, 
           everything()) %>%
    mutate_at(vars(starts_with("split")), 
              str_trim)
  
  find_my_parent <- function(node) {
    # depth-first walk on partynode structure (recursive function)
    # decision rules are extracted for every branch
    # parents and children are linked
    # because or recursion using <<- which isn't my favorite
    if (!is.terminal(node)) {
      for (i in 1:length(node)) {
        child <- node$kids[[i]]$id
        parent <- node$id
        node_table[node_table$nodeID == child, "parent"] <<- parent
        find_my_parent(node[[i]])
      }
    }
  }
  
  find_my_parent(chaidobject$node)
  
  # splitrule is hard to read make ruletext as more plain text
  node_table$ruletext <- 
    str_replace_all(node_table$splitrule,
                    c("%in%" = "is",
                      "c\\('" = "'",
                      "'\\)" = "'")
    )

  # give the inner nodes names to use later on so we don't lose track 
  # during map_dfr operation
  names(inner_nodes) <- inner_nodes

  # Create an empty split table
  split_table <- tibble::tibble()
  
  # build the split table with relevant stats
  split_table <- 
    map_dfr(
      .x = inner_nodes,
      ~ node_table %>%
        filter(parent == .x) %>%
        select(levels(original_df$outcome)) %>%
        chisq.test(correct = FALSE) %>%
        newbroom(), .id = "nodeID") %>%
    rename(
      chisq = statistic,
      rawpvalue = p.value,
      df = parameter) %>%
    select(-method)
  
  split_table$nodeID <- as.integer(split_table$nodeID)
  split_table$adjustedp <- NA_real_
  split_table$split.variable <- NA_character_
  
  for (i in inner_nodes) {
    split_table[split_table$nodeID == i, "adjustedp"] <- 
      min(unlist(nodeapply(chaidobject,
                           ids = all_nodes,
                           FUN = function(n) n$info)[[i]]
      )
      )
    split_table[split_table$nodeID == i, "split.variable"] <- 
      attr(which.min(unlist(nodeapply(chaidobject,
                                      ids = all_nodes,
                                      FUN = function(n) n$info)[[i]]
      )
      ), "names"
      )
  }
  
  split_table$split.variable <- 
    str_split(split_table$split.variable, 
              "\\.", 
              n = 2, 
              simplify = TRUE)[,2]
  
  node_table <- full_join(node_table, split_table, by = "nodeID")
  
  node_table <- 
    node_table %>% 
    select(nodeID, 
           parent, 
           NodeN, 
           levels(original_df$outcome), 
           ruletext, 
           split.variable, 
           chisq, 
           df, 
           adjustedp, 
           rawpvalue, 
           everything())
  node_table
}
