
from __future__ import print_function
from subprocess import check_output
from os import listdir
from os.path import join
from time import sleep
from utils import Colour, FoundError, getCurrentAbsolutePath, existsIn, EXEC, WHITE_LISTED_EXTENSIONS

dir_path = getCurrentAbsolutePath(__file__)

# (path/to/tests, infrared_command)
LEXER_TESTS = (join(dir_path, "lexer"), "tokenize")
PARSER_TESTS = (join(dir_path, "parser"), "parse")

jobs = [
    LEXER_TESTS,
    PARSER_TESTS
]

for job in jobs:
    print(Colour.BOLD + "\nGENERATING TESTS: " + Colour.END + job[0])
    try: 
        # We don't want to check any dotfiles in these directories
        directories = [f for f in listdir(job[0]) if f[0] != "."]
    except:
        print("Directory was not found: " + job[0])
        continue
    for path in directories:
        real_path = join(job[0], path)
        print(Colour.LIGHT_GRAY + u'\u25F4' + " BUILDING " + Colour.END + path, end="\r")
        try:
            # Find test file (we only expect 1 file at the moment)
            files = listdir(real_path)
            files_valid = [f for f in files if existsIn(WHITE_LISTED_EXTENSIONS, f[-3:])]
            if len(files_valid) != 1: raise
            file = join(real_path, files_valid[0])
            output = check_output([EXEC, job[1], file])
            # Check output for simple errors
            if (output.find("Syntax_Error") != -1 or
               output.find("Unknown_Token") != -1):
               raise FoundError
            # Create expected output file
            file_exp_name = file[:-3] + ".exp"
            file_exp = open(file_exp_name, "w")
            file_exp.write(output)
            file_exp.close()
            print(Colour.GREEN + u'\u2714' + " done " + Colour.END + path + "    ")
        except FoundError:
            print(Colour.RED + u'\u2715' + " fail " + Colour.END + path + ": " + 
                  Colour.LIGHT_GRAY + "Syntax_Error or Unknown_Token encountered" + Colour.END)
        except:
            print(Colour.RED + u'\u2715' + " error " + Colour.END + path + "    ")
