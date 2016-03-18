open C45
open Test_functions
open DataTypes

let tree = c45 (randTrainSet ())
let () = DataTypes.toDotStdout tree
