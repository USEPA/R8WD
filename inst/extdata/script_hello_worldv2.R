### pass variables defined in a session to a sourced script
# commandArgs <- function(...) 1:3
# source(system.file('extdata', 'script_hello_worldv2.R', package = 'R8WD'))
message('hello world')

message(commandArgs())
