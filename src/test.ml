open Oc45
open Test_functions

let () = Random.self_init ()

let tree = IntOc45.c45 (randTrainSet ())

let nbTests = 10000
let failed = ref 0
let () = for test = 0 to nbTests-1; do
	let cat = Random.int nbCategories in
	let data = testData cat in
	if IntOc45.classify tree data <> cat then
		failed := !failed + 1
done

let () = IntOc45.toDotStdout (fun fmt x -> Format.fprintf fmt "%d" x) tree

let () = Format.eprintf "Failed %d/%d tests.@." !failed nbTests
