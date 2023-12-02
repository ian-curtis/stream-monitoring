library(cli)

print("hey")

invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))

print("bye")