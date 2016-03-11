
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

let rec c45 trainset =
	let fsum = List.fold_left (fun cur x -> cur +. x) 0. in
	let log2 x = (log x) /. (log 2.) in

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

	let featureGain ft =
		let countFilter filter = List.fold_left
			(fun cur x -> if filter x then (cur+1) else cur) 0 in
		let rec gainLoss curLoss = function
		| -1 -> curLoss
		| v ->
			let filter = (fun tv -> snd (tv.data.(ft)) = v) in
			let count = countFilter filter trainset.set in
			let entr = entropy filter in
			gainLoss
				(curLoss +. (float_of_int count) /.
					(float_of_int trainset.setSize) *. entr)
				(v-1)
		in
		let wholeEntr = entropy (fun _ -> true) in
		wholeEntr -. gainLoss 0. (trainset.featureMax.(ft))
	in

	assert false
