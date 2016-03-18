
(******************************************************************************
 * OC4.5
 * A pure OCaml implementation of C4.5 algorithm
 *
 * By Théophile Bastian <contact@tobast.fr>
 * and Noémie Fong (aka. Minithorynque), 2016.
 ******************************************************************************
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *****************************************************************************)

type feature = int
type category = int
type dataVal = int
type data = dataVal array
type trainVal = {
	data : data ;
	category : category
}
type trainSet = {
	set : trainVal list ;
	nbFeatures : int ;
	featureMax : int array ; (* Max value for the feature a *)
	featContinuity : bool array ;
	nbCategories : int ;
	setSize : int (* number of training values in the training set *)
}

module DVMap = Map.Make (struct type t = dataVal let compare = compare end)

type decisionTree = DecisionLeaf of category
	| DecisionDiscreteNode of feature * decisionTree DVMap.t
	| DecisionContinuousNode of feature * int (* threshold *) *
			decisionTree (* lower *) * decisionTree (* upper *)


let toDot fmt (tree : decisionTree) =
	let cId = ref 0 in
	let incr r = r := !r + 1 in
	let rec printTree = function
	| DecisionLeaf cat ->
		Format.fprintf fmt "%d [label=\"Cat. %d\"]@\n" !cId cat;
		incr cId;
		!cId - 1
	| DecisionDiscreteNode(feat,children) ->
		Format.fprintf fmt "%d [shape=box,label=\"Feat %d\"]@\n" !cId feat;
		let cellId = !cId in
		incr cId;
		DVMap.iter (fun key child ->
			let ccid = printTree child in
			Format.fprintf fmt "%d -> %d [label=\"=%d\"]@\n" cellId ccid key)
			children;
		cellId
	| DecisionContinuousNode(feat, thres, low, high) ->
		let cellId = !cId in
		incr cId;
		Format.fprintf fmt "%d [shape=box,label=\"Feat %d\"]@\n" cellId feat ;
		let lowId = printTree low and highId = printTree high in
		Format.fprintf fmt "%d -> %d [label=\"<= %d\"]@\n" cellId lowId thres ;
		Format.fprintf fmt "%d -> %d [label=\"> %d\"]@\n" cellId highId thres ;
		cellId
	in
	Format.open_hovbox 4 ;
	Format.fprintf fmt "digraph decisionTree {@\n";
	let _ = printTree tree in
	Format.close_box () ;
	Format.fprintf fmt "@\n}@."

let toDotStdout = toDot Format.std_formatter
