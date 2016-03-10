
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
type dataval = feature * int
type data = dataval array
type 'a trainval = {
	data : data ;
	category : 'a
}
type 'a trainset = {
	set : 'a trainval list ;
	nbFeatures : int ;
	featureMax : int array (* Max value for the feature a *)
}

module DVMap = Map.Make (struct type t = dataval let compare = compare end)

type 'a decisiontree = DecisionLeaf of 'a
	| DecisionNode of feature * 'a decisiontree DVMap.t
