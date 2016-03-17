
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

open DataTypes

let majorityCasesThreshold = 5

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
			Array.fold_left (fun cur a -> cur -. (rat a) *. log2 (rat a)) 0. tab
		in
		let totInfo = entropyWithTab leftFreq !leftCard in
		let addCell tab id v = tab.(id) <- tab.(id) + v in
		let nextEntropy () = (match !sorted with
			| _::[] | [] -> raise Not_found
			| head::(hd2::_ as tl) ->
				sorted := tl ;
				let catChanged = head.category in
				addCell leftFreq catChanged (-1) ;
				addCell rightFreq catChanged 1 ;
				leftCard := !leftCard - 1 ;
				(hd2.data.(ft) + head.data.(ft)) / 2,
					totInfo -. (entropyWithTab leftFreq !leftCard) -.
						(entropyWithTab rightFreq
							(trainset.setSize - !leftCard))
			)
		in
		let rec bestPiv curMax curMaxPiv =
			(try
				let piv,entr = nextEntropy () in
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

	if trainset.setSize < majorityCasesThreshold then
		(* #trainset < threshold => insert the majority vote leaf.
			In case there is no majority, the result is an abritrary choice. *)
		DecisionLeaf(majorityVote (List.map
			(fun tv -> tv.category) trainset.set))
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
				(fun (i,x) (j,y) -> if y > x then (j,y) else (i,x))
				(-1,-1.)
				(List.map (fun i -> i,featureGainRatio i)
					(0 <|> trainset.nbFeatures))
				in

			if trainset.featContinuity.(maxGainFeature) then begin
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
					) (emptyset,emptyset) emptyset.set in
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

