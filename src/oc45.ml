
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

(*********************** DATA TYPES ******************************************)
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

(* Note that featureMax will be inferred *)
let emptyTrainSet nbFeatures nbCategories featContinuity  = 
    {set = [] ;
    nbFeatures = nbFeatures ;
    featureMax = Array.make nbFeatures 0 ;
    featContinuity = featContinuity ;
    nbCategories = nbCategories ;
    setSize = 0
    }
    
let addData trainVal trainSet =
    for feat = 0 to (trainSet.nbFeatures - 1) do
        trainSet.featureMax.(feat) <- max trainSet.featureMax.(feat) trainVal.data.(feat)
    done;
    {set = trainVal :: trainSet.set ;
    nbFeatures = trainSet.nbFeatures ;
    featureMax = trainSet.featureMax ;
    featContinuity = trainSet.featContinuity ;
    nbCategories = trainSet.nbCategories ;
    setSize = trainSet.setSize + 1
    }

let setFeatureMax feat maxVal trainSet =
    trainSet.featureMax.(feat) <- maxVal

(* get functions *)
let getSet trainSet =
    trainSet.set

let getNbFeatures trainSet = 
    trainSet.nbFeatures 

let getFeatureMax trainSet =
    trainSet.featureMax

let getFeatContinuity trainSet =
    trainSet.featContinuity

let getNbCategories trainSet =
    trainSet.nbCategories 

let getSetSize trainSet =
    trainSet.setSize 

(* graph generation *)
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
(******************* END DATA TYPES ******************************************)

let majorityCasesThreshold = 5
let epsilonGain = 0.000001

let (<|>) a b =
	(** a|b : generates the list [a ; a+1 ; ... ; b-1] *)
	let rec span b cur =
		if a = b then a::cur
			else span (b-1) (b::cur)
	in span (b-1) []
let bxor a b = match a,b with
| true,true | false,false -> false
| _,_ -> true

module IMap = Map.Make(struct type t=int let compare = compare end)

let majorityVote l =
	(** Returns the most present value in l. If the maximum is not unique,
		returns an arbitrary value among the possible ones. *)
	let counts = List.fold_left
		(fun map x -> IMap.add x
			((try IMap.find x map with Not_found -> 0) + 1) map)
		IMap.empty l in
	let _,maxarg = IMap.fold (fun arg v (cMax,cArg) ->
		if v > cMax then (v,arg) else (cMax,cArg)) counts (-1,-1) in
	maxarg

(* classify data based on a decision tree *)
let rec classify tree data = match tree with
    | DecisionLeaf category -> category
    | DecisionDiscreteNode (feat, decisionTreeMap) ->
        classify (DVMap.find data.(feat) decisionTreeMap) data
    | DecisionContinuousNode (feat, thresh, lowerTree, upperTree) ->
        if data.(feat) < thresh 
            then classify lowerTree data
            else classify upperTree data 
    
    

let rec c45 trainset =
	let fsum = List.fold_left (fun cur x -> cur +. x) 0. in
	let log2 x = (log x) /. (log 2.) in

	let countFilter filter = List.fold_left
		(fun cur x -> if filter x then (cur+1) else cur) 0 in

	let entropy filter =
		let catCount = Array.make (trainset.nbCategories) 0 in
		let nbTrainVal = ref 0 in
		List.iter (fun tv -> if filter tv then begin
			nbTrainVal := !nbTrainVal + 1 ;
			catCount.(tv.category) <- catCount.(tv.category) + 1 end)
			trainset.set ;
		-1. *. fsum (List.map (fun k ->
					let x = (float_of_int k) /. (float_of_int !nbTrainVal) in
					x *. (log2 x))
				(Array.to_list catCount))
	in

	let contGains = Array.make (trainset.nbFeatures) 0. in
	let findContThreshold ft =
		let sorted=ref (List.sort
			(fun tv1 tv2 -> tv1.data.(ft) - tv2.data.(ft)) trainset.set) in
		let leftCard = ref trainset.setSize in
		let leftFreq = Array.make (trainset.nbCategories) 0
		and rightFreq= Array.make (trainset.nbCategories) 0 in
		List.iter (fun x -> leftFreq.(x.category) <- leftFreq.(x.category)+1)
			trainset.set;
		let entropyWithTab tab card =
			let fcard = float_of_int card in
			let rat = fun a -> (float_of_int a) /. fcard in
			Array.fold_left (fun cur a -> (match a with
				| 0 -> cur
				| a -> cur -. (rat a) *. log2 (rat a))) 0. tab
		in
		let totInfo = entropyWithTab leftFreq !leftCard in
		let addCell tab id v = tab.(id) <- tab.(id) + v in
		let splitVal card =
			let oneSide c = (match c with
			| 0 -> 0.
			| c ->
				let fcard = float_of_int c in
				let rat = fcard /. (float_of_int trainset.setSize) in
				-. (rat *. (log2 rat))
			) in
			(oneSide card) +. (oneSide (trainset.setSize - card))
		in

		let nextInfoGain () = (match !sorted with
			| _::[] | [] -> raise Not_found
			| head::(hd2::_ as tl) ->
				sorted := tl ;
				let catChanged = head.category in
				addCell leftFreq catChanged (-1) ;
				addCell rightFreq catChanged 1 ;
				leftCard := !leftCard - 1 ;
				let gain = totInfo -. (entropyWithTab leftFreq !leftCard) -.
					(entropyWithTab rightFreq
							(trainset.setSize - !leftCard)) in
				(hd2.data.(ft) + head.data.(ft)) / 2,
					gain /. (splitVal !leftCard)
			)
		in
		let rec bestPiv curMax curMaxPiv =
			(try
				let piv,entr = nextInfoGain () in
				if entr > curMax then
					bestPiv entr piv
				else bestPiv curMax curMaxPiv
			with Not_found ->
				curMaxPiv,curMax)
		in

		let piv,gain = bestPiv 0. (-1) in
		contGains.(ft) <- gain ;
		piv
	in
	let contThresholds = Array.init (trainset.nbFeatures) findContThreshold in

	let featureGainRatio ft =
		let rec gainLoss curLoss curSplit = function
		| -1 -> curLoss,curSplit
		| v ->
			let filter = (fun tv -> tv.data.(ft) = v) in
			let count = countFilter filter trainset.set in
			let fcountrat = (float_of_int count) /.
				(float_of_int trainset.setSize) in
			let entr = entropy filter in
			gainLoss
				(curLoss +. fcountrat *. entr)
				(curSplit +. fcountrat *. log2 fcountrat)
				(v-1)
		in

		(match trainset.featContinuity.(ft) with
		| true -> contGains.(ft)
		| false ->
			let wholeEntr = entropy (fun _ -> true) in
			let loss,spl = gainLoss 0. 0. (trainset.featureMax.(ft)) in
			(wholeEntr -. loss) /. (-.spl)
		)
	in

	let majorityLeaf () =
		(* In case there is no majority, the result is an abritrary choice. *)
		DecisionLeaf(majorityVote (List.map
			(fun tv -> tv.category) trainset.set))
	in

	if trainset.setSize < majorityCasesThreshold then
		(* #trainset < threshold => insert the majority vote leaf. *)
		majorityLeaf ()
	else begin
		let commonClass = List.fold_left
			(fun cur x -> if x.category = cur then cur else -1)
			((List.hd trainset.set).category)
			(List.tl trainset.set) in

		if commonClass >= 0 then
			(* Each trainVal has the same category: insert a leaf *)
			DecisionLeaf(commonClass)
		else begin
			let maxGainFeature,maxGain = List.fold_left
				(fun (i,x) (j,y) ->
					if y > x then (j,y) else (i,x))
				(-1,-1.)
				(List.map (fun i -> i,featureGainRatio i)
					(0 <|> trainset.nbFeatures))
				in

			if maxGain < epsilonGain then
				majorityLeaf ()
			else if trainset.featContinuity.(maxGainFeature) then begin
				let threshold = contThresholds.(maxGainFeature) in
				let emptyset = { trainset with set = [] ; setSize = 0 } in
				let lower, upper = List.fold_left (fun (lset,uset) tv ->
					if tv.data.(maxGainFeature) < threshold then
						{ lset with
							set = tv::lset.set ;
							setSize = lset.setSize+1 }, uset
					else
						lset, {uset with
							set = tv::uset.set ;
							setSize = uset.setSize+1 }
					) (emptyset,emptyset) trainset.set in
				DecisionContinuousNode
					(maxGainFeature, threshold, c45 lower, c45 upper)
			end else begin
				let submap = List.fold_left (fun map v ->
					let sset = List.filter
						(fun tv -> tv.data.(maxGainFeature) = v)
						trainset.set in
					DVMap.add v (c45 { trainset with
							set = sset ;
							setSize = List.length sset
						}) map)
						DVMap.empty
						(0<|>(trainset.featureMax.(maxGainFeature)+1)) in
				DecisionDiscreteNode (maxGainFeature,submap)
			end
		end
	end

