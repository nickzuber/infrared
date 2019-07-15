#!/bin/bash

rgxFindFn="and (string_of_statement|string_of_expression) "
rgxFindUnfinishedMatchStmt=":space:+| ([a-zA-Z]+) _ ->"
rgxFindFinishedMatchStmt=":space:+| ([a-zA-Z]+)"
rgxIsValidBlock="^[A-Z]"

begin=0
pass=0
fail=0

filePrinter="./InfraredParser/Printer.ml"

fnPrinterCoverage () {
  while read -r line
  do
    if [[ begin -eq 1 && -z $line ]]; then
      [[ $fail -eq 0 ]]\
        && local color="\033[32;1m"\
        || local color="\033[31;1m"
      local total=$((fail + pass))
      echo "\n$color$pass/$total passing\033[39;0m"
      pass=0
      fail=0
      begin=0
    elif [[ $line =~ $rgxFindFn ]]; then
      echo "\033[1m\n${BASH_REMATCH[1]}\033[0m"
      begin=1
    elif [[ begin -eq 1 && $line =~ $rgxFindUnfinishedMatchStmt ]]; then
      local name=${BASH_REMATCH[1]}
      if [[ -n $name ]]; then
        echo " \033[90m↺ $name\033[39;0m"
        ((fail++))
      fi
    elif [[ begin -eq 1 && $line =~ $rgxFindFinishedMatchStmt ]]; then
      local name=${BASH_REMATCH[1]}
      if [[ -n $name && $name =~ $rgxIsValidBlock ]]; then
        echo " \033[32;1m✓\033[39;0m $name"
        ((pass++))
      fi
    fi
    local name=""
  done < "$filePrinter"
}

fnPrinterCoverage
