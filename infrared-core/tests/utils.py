
import os

class FoundError(Exception):
    pass

class FailedTest(Exception):
    pass

class IncompleteTest(Exception):
    pass

class Colour:
    END = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    BLUE = '\033[34m'
    GREEN = '\033[32m'
    YELLOW = '\033[33m'
    RED = '\033[31m'
    GRAY = '\033[30m'
    LIGHT_GRAY = '\033[90m'

def getCurrentAbsolutePath(_file_):
    return os.path.dirname(os.path.realpath(_file_))

def existsIn(lst, str):
    try:
        lst.index(str)
        return True
    except:
        return False

# Path to infrared executable
EXEC = os.path.join(os.path.dirname(__file__), '..') + "/infrared.native"

# Make sure to include the `.` prefix
WHITE_LISTED_EXTENSIONS = [".js"]
