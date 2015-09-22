#!/usr/bin/env tclsh
# Example code
set example {1 2 3}
e.g. {set'add example 4} -> {1 2 3 4}
e.g. {set'add example 4} -> {1 2 3 4}
book open "name \"substr\""
proc set'remove {_set args} {
   upvar 1 $_set set
   foreach el $args {
       set pos [lsearch -exact $set $el]
   }
}
