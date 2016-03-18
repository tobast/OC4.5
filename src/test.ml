open Oc45
open Test_functions

let tree = c45 (randTrainSet ())
let () = toDotStdout tree
