
from __future__ import print_function
from subprocess import check_output
from os import listdir
from os.path import join
from time import sleep
from utils import Colour, FoundError, FailedTest, getCurrentAbsolutePath, existsIn, EXEC, WHITE_LISTED_EXTENSIONS

dir_path = getCurrentAbsolutePath(__file__)

# (path/to/tests, infrared_command)
LEXER_TESTS = (join(dir_path, "lexer"), "tokenize")
PARSER_TESTS = (join(dir_path, "parser"), "parse")

jobs = [LEXER_TESTS, PARSER_TESTS]

for job in jobs:
    print(Colour.BOLD + "\nRUNNING TESTS: " + Colour.END + job[0])
    try: 
        # We don't want to check any dotfiles in these directories
        directories = [f for f in listdir(job[0]) if f[0] != "."]
    except:
        print("Directory was not found: " + job[0])
        continue
    for path in directories:
        real_path = join(job[0], path)
        print(Colour.LIGHT_GRAY + u'\u25F4' + " RUNNING " + Colour.END + path, end="\r")
        try:
            # Find test file (we only expect 1 file at the moment)
            files = listdir(real_path)
            files_valid = [f for f in files if existsIn(WHITE_LISTED_EXTENSIONS, f[-3:])]
            if len(files_valid) != 1: raise
            file = join(real_path, files_valid[0])
            actual = check_output([EXEC, job[1], file])
            # Read expected output file
            file_exp_name = file[:-3] + ".exp"
            file_exp = open(file_exp_name, "r")
            expected = file_exp.read()
            file_exp.close()
            # shitty placeholder do better pls ty
            if (len(actual) != len(expected)):
                raise FailedTest({"actual": len(actual), "expected": len(expected)})
            print(Colour.GREEN + u'\u2714' + " PASS " + Colour.END + path + "    ")
        except FailedTest as e:
            obj = e.args[0]
            print(Colour.RED + u'\u2715' + " FAIL " + Colour.END + path + ": " +
                  Colour.LIGHT_GRAY + str(obj["actual"]) + ", " + str(obj["expected"]) + Colour.END)
        except:
            print(Colour.RED + u'\u2715' + " ERROR " + Colour.END + path + "    ")
