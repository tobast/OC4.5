open C45
open Test_functions

let tree = c45 (randTrainSet ())
let () = Format.printf "Done.@."
