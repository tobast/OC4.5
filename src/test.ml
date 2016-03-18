open Oc45
open Test_functions

let testedCat = 0

let tree = c45 (randTrainSet ())
let data = testData testedCat
let () = if classify tree data != testedCat then failwith "Test failure"

let () = toDotStdout tree
