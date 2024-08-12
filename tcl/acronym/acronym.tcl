proc abbreviate {phrase} {
    variable matches [regexp -all -inline {([[:alpha:]])[[:alpha:]']*} $phrase]

    variable res {}
    foreach {_ first} $matches {append res [string toupper $first]}
    set res
}
