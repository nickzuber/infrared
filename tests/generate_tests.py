
from subprocess import check_output
from os import listdir

EXEC = "../infrared.native"
LEXER_TESTS = "./lexer"
PARSER_TESTS = "./parser"

# @TEST
# file = "./parser/keywords/keywords.js"

directories = listdir(LEXER_TESTS)
for path in directories:
    output = check_output([EXEC, "parse", file])

# output = check_output([EXEC, "parse", file])
