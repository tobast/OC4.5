
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
type dataVal = feature * int
type data = dataVal array
type trainVal = {
	data : data ;
	category : category
}
type trainSet = {
	set : trainVal list ;
	nbFeatures : int ;
	featureMax : int array ; (* Max value for the feature a *)
	nbCategories : int ;
	setSize : int (* number of training values in the training set *)
}

module DVMap = Map.Make (struct type t = dataVal let compare = compare end)

type decisionTree = DecisionLeaf of category
	| DecisionNode of feature * decisionTree DVMap.t
