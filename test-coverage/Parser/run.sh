#!/bin/bash

rgxFindFn="and (string_of_statement|string_of_expression) "
rgxFindUnfinishedMatchStmt=":space:+| ([a-zA-Z]+) _ ->"
rgxFindFinishedMatchStmt=":space:+| ([a-zA-Z]+)"
rgxIsValidBlock="^[A-Z]"

flag=$1
begin=0
suites=0
suites_failed=0
pass=0
fail=0
fail_local=0

filePrinter="./InfraredParser/Printer.ml"

print_if_verbose () {
  if [[ "$flag" = "--verbose" || "$flag" = "-v" ]]; then
    echo $1
  fi
}

fn_printer_covereage () {
  while read -r line
  do
    # You were testing a suite and it had just finished.
    if [[ begin -eq 1 && -z $line ]]; then
      if [[ fail_local -gt 0 ]]; then
        ((suites_failed++))
      fi
      begin=0
    # You have found a new test suite.
    elif [[ $line =~ $rgxFindFn ]]; then
      print_if_verbose ""
      echo "\033[42;38;5;16;1m TEST \033[49;39;0m \033[32;2mInfraredParser/Printer.ml#\033[39;0m\033[1m${BASH_REMATCH[1]}\033[0m"
      begin=1
      pass_local=0
      fail_local=0
      ((suites++))
    # You have found an incomplete method within the test suite.
    elif [[ begin -eq 1 && $line =~ $rgxFindUnfinishedMatchStmt ]]; then
      local name=${BASH_REMATCH[1]}
      if [[ -n $name ]]; then
        print_if_verbose " \033[90m↺ $name\033[39;0m"
        ((fail++))
        ((fail_local++))
      fi
    # You have found a complete method within the test suite.
    elif [[ begin -eq 1 && $line =~ $rgxFindFinishedMatchStmt ]]; then
      local name=${BASH_REMATCH[1]}
      if [[ -n $name && $name =~ $rgxIsValidBlock ]]; then
        print_if_verbose " \033[32;1m✓\033[39;0m $name"
        ((pass++))
      fi
    fi
    local name=""
  done < "$filePrinter"
}

# Run all the functions for coverage.
fn_printer_covereage

# Print the final coverage report.
total=$((pass+fail))
percentage=$((100*$pass/$total))
percentage_exact=$(echo "$pass/$total*100" | bc -l)
echo ""
if [[ suites_failed -gt 0 ]]; then
  echo "\033[97;1mTest suites:\033[39m\033[38;38;5;1m $suites_failed failed\033[0;0m, $suites total"
else
  echo "\033[97;1mTest suites:\033[39m\033[32;1m $((suites-suites_failed)) passed\033[0;0m, $suites total"
fi
echo "\033[97;1mTests:      \033[39m\033[32;1m $pass passed\033[0;0m, $total total"
echo "\033[97;1mCoverage:   \033[0m $percentage% / 100%"
echo "\033[90mRan all covereage functions matching \033[0m/$rgxFindFn/i"
echo "\033[32;1minfrared-coverage-reporter >> Report generaged (@TODO) \033[0;0m"
