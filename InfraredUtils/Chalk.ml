let bold str = Printf.sprintf "\x1b[1m%s\x1b[0m" str
let underline str = Printf.sprintf "\x1b[4m%s\x1b[0m" str
let invert str = Printf.sprintf "\x1b[7m%s\x1b[0m" str

let red str = Printf.sprintf "\x1b[31m%s\x1b[39m" str
let green str = Printf.sprintf "\x1b[32m%s\x1b[39m" str
let yellow str = Printf.sprintf "\x1b[33m%s\x1b[39m" str
let blue str = Printf.sprintf "\x1b[34m%s\x1b[39m" str
let magenta str = Printf.sprintf "\x1b[35m%s\x1b[39m" str
let cyan str = Printf.sprintf "\x1b[36m%s\x1b[39m" str
let gray str = Printf.sprintf "\x1b[90m%s\x1b[39m" str
let white str = Printf.sprintf "\x1b[97m%s\x1b[39m" str

let light_gray str = Printf.sprintf "\x1b[37m%s\x1b[39m" str
let light_red str = Printf.sprintf "\x1b[91m%s\x1b[39m" str
let light_green str = Printf.sprintf "\x1b[92m%s\x1b[39m" str
let light_yellow str = Printf.sprintf "\x1b[93m%s\x1b[39m" str
let light_blue str = Printf.sprintf "\x1b[94m%s\x1b[39m" str
let light_magenta str = Printf.sprintf "\x1b[95m%s\x1b[39m" str
let light_cyan str = Printf.sprintf "\x1b[96m%s\x1b[39m" str

let bg_red str = Printf.sprintf "\x1b[41m%s\x1b[49m" str
let bg_green str = Printf.sprintf "\x1b[42m%s\x1b[49m" str
let bg_yellow str = Printf.sprintf "\x1b[43m%s\x1b[49m" str
let bg_blue str = Printf.sprintf "\x1b[44m%s\x1b[49m" str
let bg_magenta str = Printf.sprintf "\x1b[45m%s\x1b[49m" str
let bg_cyan str = Printf.sprintf "\x1b[46m%s\x1b[49m" str
let bg_gray str = Printf.sprintf "\x1b[100m%s\x1b[49m" str
let bg_white str = Printf.sprintf "\x1b[107m%s\x1b[49m" str

let bg_light_gray str = Printf.sprintf "\x1b[47m%s\x1b[49m" str
let bg_light_red str = Printf.sprintf "\x1b[101m%s\x1b[49m" str
let bg_light_green str = Printf.sprintf "\x1b[102m%s\x1b[49m" str
let bg_light_yellow str = Printf.sprintf "\x1b[103m%s\x1b[49m" str
let bg_light_blue str = Printf.sprintf "\x1b[104m%s\x1b[49m" str
let bg_light_magenta str = Printf.sprintf "\x1b[105m%s\x1b[49m" str
let bg_light_cyan str = Printf.sprintf "\x1b[106m%s\x1b[49m" str
